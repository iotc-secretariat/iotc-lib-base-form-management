#'Reads a form 1-RC (intermediate format) and returns the information contained in its METADATA and DATA sections
#'@export
read_form_NC = function(filename) {
  meta = as.data.table(read.xlsx(filename, sheet = "Metadata",               detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = as.data.table(read.xlsx(filename, sheet = "Data",     startRow = 2, detectDates = TRUE, skipEmptyRows = TRUE,  skipEmptyCols = TRUE))

  form_type    = meta[2][[4]]
  form_version = meta[2][[7]]

  if(is.na(form_type)) stop("Form type is unavailable")
  if(form_type  != "1-RC") stop(paste0("Form type is invalid (should be: 1-RC, is: ", form_type, ")"))

  data_columns = c("QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE", "RETAINED_REASON_CODE")
  species_data_columns = colnames(data)[c(10:length(colnames(data)))]

  columns = append(data_columns, species_data_columns)

  colnames(data) = columns

  data = check_and_remove_extra_rows(data)

  DFC = 1
  DLC = length(data_columns)
  SFC = DLC + 1
  SLC = length(colnames(data))

  data = data.table(data[, DFC:DLC], sapply(data[, SFC:SLC, with = FALSE], as.numeric))

  data = fix_repeated_species(data, check_repeated_species(colnames(data)))

  version   = as.numeric(meta[2][[7]])

  fp_name   = meta[7][[4]]
  fp_mail   = meta[8][[4]]

  org_name   = meta[7][[7]]
  org_mail   = meta[8][[7]]

  finalization_date = sanitize_date(meta[10][[4]])
  submission_date   = sanitize_date(meta[11][[4]])

  year      = as.integer(   meta[19][[4]])
  unit      = "MT" #sanitize_code(meta[10]$X19)
  fleet     = sanitize_code(meta[16][[4]])
  rep_fleet = sanitize_code(meta[17][[4]])

  comments = meta[24][[3]]

  metadata = list(
    VERSION = version,
    FOCAL_POINT_NAME = fp_name,
    FOCAL_POINT_MAIL = fp_mail,
    ORGANIZATION_NAME= org_name,
    ORGANIZATION_MAIL= org_mail,
    FINALIZATION_DATE = finalization_date,
    SUBMISSION_DATE = submission_date,
    YEAR = year,
    FLEET = fleet,
    REP_FLEET = rep_fleet,
    CATCH_UNIT = unit,
    COMMENTS = comments
  )

  if(is.na(fleet)) fleet_code = rep_fleet
  else if(is.na(rep_fleet)) fleet_code = fleet
  else fleet_code = ifelse(fleet == rep_fleet, fleet, paste0(fleet, "-", rep_fleet))

  data = data.table(FLEET_CODE = fleet_code, YEAR = year, data)

  data =
    melt(
      data,
      id.vars       = c("FLEET_CODE", "YEAR", "QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE", "RETAINED_REASON_CODE"),
      variable.name =   "SPECIES_CODE",
      value.name    =   "CATCH"
    )

  data[, IOTC_FISHERY_CODE   := sanitize_code(IOTC_FISHERY_CODE)]
  data[, FISHING_GROUND_CODE := sanitize_code(FISHING_GROUND_CODE)]
  data[, DATA_SOURCE_CODE    := sanitize_code(DATA_SOURCE_CODE)]
  data[, DATA_PROCESSING_CODE:= sanitize_code(DATA_PROCESSING_CODE)]
  data[, TARGET_SPECIES_CODE := sanitize_code(TARGET_SPECIES_CODE)]
  data[, COVERAGE_CODE       := sanitize_code(COVERAGE_CODE)]
  data[, RETAINED_REASON_CODE:= sanitize_code(RETAINED_REASON_CODE)]
  data[, SPECIES_CODE        := sanitize_code(SPECIES_CODE)]
  data[, CATCH_UNIT_CODE     := unit]

  data = merge(data, load_species(), by.x = "SPECIES_CODE", by.y = "CODE", all.x = TRUE)[!is.na(CATCH)]

  data$CATCH = round(as.numeric(data$CATCH), 3)

  #data = data[CATCH > 0][order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]
  data = data[order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]

  data$YEAR    = as.integer(data$YEAR)
  data$QUARTER = as.integer(data$QUARTER)

  data = data[, .(FLEET_CODE, YEAR, QUARTER, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, CATCH, CATCH_UNIT_CODE,
                  SPECIES_CODE, NAME_LT, NAME_EN, IS_IOTC, IS_AGGREGATE, IS_SSI, IS_BAIT, WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE, SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]

  return(
    list(
      META = metadata,
      DATA = data
    )
  )
}

analyze_form_NC = function(metadata, data) {
  return(
    list(
      METADATA = analyze_form_NC_metadata(metadata),
      DATA     = analyze_form_NC_data(data),
    )
  )
}

analyze_form_NC_metadata = function(metadata) {
  return(
    list(NO_YEAR      = is.na(metadata$YEAR),
         NO_FLEET     = is.na(metadata$FLEET),
         NO_REP_FLEET = is.na(metadata$REP_FLEET),
         NO_CATCH_UNIT= is.na(metadata$CATCH_UNIT)
    )
  )
}

analyze_form_NC_data = function(data) {
  data[is.na(CATCH), CATCH := 0]

  SP_unknown  = unique(data[is.na(NAME_LT) & is.na(NAME_EN)]$SPECIES_CODE)
  FI_unknown  = unique(data[is.na(IOTC_FISHERY_CODE) & !is.na(CATCH) & CATCH > 0]$FISHERY_CODE)

  SP_no_catch = unique(data[, .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE)][is.na(CATCH) | CATCH == 0]$SPECIES_CODE)
  FI_no_catch = unique(data[, .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE)][is.na(CATCH) | CATCH == 0]$IOTC_FISHERY_CODE)

  Q_FI_FG_SP_duplicates =
    data[, .(RECORDS = .N), keyby = .(QUARTER, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, SPECIES_CODE)][RECORDS > 1]

  Q_FI_FG_incomplete =
    unique(
      data[
        is.na(QUARTER) |
        is.na(IOTC_FISHERY_CODE) |
        is.na(FISHING_GROUND_CODE) |
        is.na(PRELIMINARY) |
        is.na(DATA_SOURCE_CODE) |
        is.na(DATA_PROCESSING_CODE) |
        is.na(TARGET_SPECIES_CODE) |
        is.na(COVERAGE_CODE) |
        is.na(RETAINED_REASON_CODE),
        .(QUARTER, FISHING_GROUND_CODE, IOTC_FISHERY_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE)
      ]
    )

  return(
    list(
      TOTAL_CATCHES           = sum(data$CATCH),
      PRELIMINARY_CATCHES     = sum(data[PRELIMINARY == "PRE"]$CATCH),
      FINAL_CATCHES           = sum(data[PRELIMINARY != "PRE"]$CATCH),
      IOTC_CATCHES            = sum(data[IS_IOTC == TRUE ]$CATCH),
      NON_IOTC_CATCHES        = sum(data[IS_IOTC == FALSE]$CATCH),
      UNKNOWN_CATCHES         = sum(data[is.na(IS_IOTC)]$CATCH),
      SPECIES_AGGR_CATCHES    = sum(data[IS_AGGREGATE == TRUE]$CATCH),
      NUM_SPECIES             = length(unique(data[!is.na(SPECIES_CODE)]$SPECIES_CODE)),
      NUM_FISHERIES           = length(unique(data[!is.na(IOTC_FISHERY_CODE)]$IOTC_FISHERY_CODE)),
      UNKNOWN_SPECIES         = SP_unknown,
      SPECIES_WITH_NO_CATCH   = SP_no_catch,
      UNKNOWN_FISHERIES       = FI_unknown,
      FISHERIES_WITH_NO_CATCH = FI_no_catch,
      CATCH_BY_SPECIES        = data[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE, NAME_EN, SPECIES_CATEGORY_CODE, IS_IOTC)],
      CATCH_BY_FISHERY        = data[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE)],
      DUPLICATE_STRATA        = Q_FI_FG_SP_duplicates,
      INCOMPLETE_STRATA       = Q_FI_FG_incomplete
    )
  )
}

#'Analyzes the processed content of a form 1-RC (intermediate format) and returns the analysis results for both the METADATA and DATA sections
#'@export
analyze_form_NC = function(processed_form) {
  META = analyze_form_NC_metadata(processed_form$META)
  DATA = analyze_form_NC_data(processed_form$DATA)

  return(append(META, DATA))
}

#'Reads a form 1-RC (intermediate format) and returns the analysis results for both the METADATA and DATA sections
#'@export
read_and_analyze_form_NC = function(filename) {
  return(analyze_form_NC(read_form_NC(filename)))
}
