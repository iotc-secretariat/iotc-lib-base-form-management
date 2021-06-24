is_form_new = function(filename) {
    tryCatch({
      as.data.table(read.xlsx(filename, sheet = "Metadata", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

      return(TRUE)
    }, error = function(e) { return(FALSE) })
}

is_form_last = function(filename) {
  tryCatch({
    DATA = as.data.table(read.xlsx(filename, sheet = "NC", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

    FORM = DATA[2][[6]]
    return(!is.na(FORM) & FORM == "IOTC FORM 1: NOMINAL CATCH")
  }, error = function(e) { return(FALSE) })
}

is_form_old = function(filename) {
  tryCatch({
    DATA = as.data.table(read.xlsx(filename, sheet = "NC", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

    FORM = DATA[2][[26]]
    return(!is.na(FORM) & FORM == "IOTC FORM 1: NOMINAL CATCH")
  }, error = function(e) { return(FALSE) })
}

#'Reads a form 1-RC (intermediate format) and returns the information contained in its METADATA and DATA sections
#'@export
read_form_NC = function(filename) {
  if(is.na(filename) | filename == "") return(list(META = NA, DATA = NA))

  if(is_form_new(filename)) return(read_form_NC_new(filename))
  else if(is_form_last(filename)) return(read_form_NC_last(filename))
  else if(is_form_old(filename)) return(read_form_NC_old(filename))

  stop("Unable to process input file: format is not recognized")
}

read_form_NC_new = function(filename) {
  meta = as.data.table(read.xlsx(filename, sheet = "Metadata",               detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = as.data.table(read.xlsx(filename, sheet = "Data",     startRow = 2, detectDates = TRUE, skipEmptyRows = TRUE,  skipEmptyCols = TRUE))

  form_type    = meta[2][[4]]
  form_version = as.numeric(meta[2][[7]])

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

  fp_name   = meta[7][[4]]
  fp_mail   = meta[8][[4]]

  org_name   = meta[7][[7]]
  org_mail   = meta[8][[7]]

  finalization_date = sanitize_date(meta[10][[4]])
  submission_date   = sanitize_date(meta[11][[4]])

  year      = as.integer(   meta[19][[4]])
  unit      = "MT" #sanitize_code(meta[10]$X19)
  flag_country= sanitize_code(meta[16][[4]])
  rep_country = sanitize_code(meta[17][[4]])

  comments = meta[24][[3]]

  metadata = list(
    VERSION = form_version,
    FOCAL_POINT_NAME = fp_name,
    FOCAL_POINT_MAIL = fp_mail,
    ORGANIZATION_NAME= org_name,
    ORGANIZATION_MAIL= org_mail,
    FINALIZATION_DATE = finalization_date,
    SUBMISSION_DATE = submission_date,
    YEAR = year,
    FLAG_COUNTRY = flag_country,
    REP_COUNTRY  = rep_country,
    CATCH_UNIT = unit,
    COMMENTS = comments
  )

  data = data.table(FLAG_COUNTRY_CODE = flag_country, REP_COUNTRY_CODE = rep_country, YEAR = year, data)

  data =
    melt(
      data,
      id.vars       = c("FLAG_COUNTRY_CODE", "REPORTING_COUNTRY_CODE", "YEAR", "QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE", "RETAINED_REASON_CODE"),
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

  SPECIES   = load_species()
  FISHERIES = load_iotc_fisheries()

  data = merge(data, SPECIES,   by.x = "SPECIES_CODE",      by.y = "CODE", all.x = TRUE)
  data = merge(data, FISHERIES, by.x = "IOTC_FISHERY_CODE", by.y = "IOTC_FISHERY_CODE", all.x = TRUE)[!is.na(CATCH)]

  data$CATCH = round(as.numeric(data$CATCH), 3)

  #data = data[CATCH > 0][order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]
  data = data[order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]

  data$YEAR    = as.integer(data$YEAR)
  data$QUARTER = as.integer(data$QUARTER)

  data = data[, .(FLAG_COUNTRY_CODE, REPORTING_COUNTRY_CODE, YEAR, QUARTER, IOTC_FISHERY_CODE, IOTC_FISHERY_NAME, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, CATCH, CATCH_UNIT_CODE,
                  SPECIES_CODE, NAME_LT, NAME_EN, IS_IOTC, IS_AGGREGATE, IS_SSI, IS_BAIT, WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE, SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]

  return(
    list(
      META = metadata,
      DATA = data
    )
  )
}

read_form_NC_last = function(filename) {
  all  = as.data.table(read.xlsx(filename, sheet = "NC", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = all[27:nrow(all)]

  form_type    = "1-RC"
  form_version = "Rev20"

  data_columns = c("QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE", "RETAINED_REASON_CODE")
  species_data_columns = all[25, 10:length(colnames(all))]
  colnames(species_data_columns) = NULL
  species_data_columns = unlist(species_data_columns)

  columns = append(data_columns, species_data_columns)

  colnames(data) = columns

  data = check_and_remove_extra_rows(data)

  DFC = 1
  DLC = length(data_columns)
  SFC = DLC + 1
  SLC = length(colnames(data))

  data = data.table(data[, DFC:DLC], sapply(data[, SFC:SLC, with = FALSE], as.numeric))

  data = fix_repeated_species(data, check_repeated_species(colnames(data)))

  fp_name   = all[ 9][[2]]
  fp_mail   = all[10][[2]]

  org_name   = all[ 9][[17]]
  org_mail   = all[10][[17]]

  finalization_date = as.Date(NA)
  submission_date   = as.Date(NA)

  year      = as.integer(   all[15][[19]])
  unit      = sanitize_code(all[16][[19]])
  flag_country= sanitize_code(all[16][[ 3]])
  rep_country = sanitize_code(all[15][[ 3]])

  comments = all[19][[2]]

  metadata = list(
    VERSION = form_version,
    FOCAL_POINT_NAME = fp_name,
    FOCAL_POINT_MAIL = fp_mail,
    ORGANIZATION_NAME= org_name,
    ORGANIZATION_MAIL= org_mail,
    FINALIZATION_DATE = finalization_date,
    SUBMISSION_DATE = submission_date,
    YEAR = year,
    FLAG_COUNTRY = flag_country,
    REP_COUNTRY  = rep_country,
    CATCH_UNIT = unit,
    COMMENTS = comments
  )

  data = data.table(FLAG_COUNTRY_CODE = flag_country, REP_COUNTRY_CODE = rep_country, YEAR = year, data)

  data =
    melt(
      data,
      id.vars       = c("FLAG_COUNTRY_CODE", "REP_COUNTRY_CODE", "YEAR", "QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE", "RETAINED_REASON_CODE"),
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

  SPECIES   = load_species()
  FISHERIES = load_iotc_fisheries()

  data = merge(data, SPECIES,   by.x = "SPECIES_CODE",      by.y = "CODE", all.x = TRUE)
  data = merge(data, FISHERIES, by.x = "IOTC_FISHERY_CODE", by.y = "IOTC_FISHERY_CODE", all.x = TRUE)[!is.na(CATCH)]

  data$CATCH = round(as.numeric(data$CATCH), 3)

  #data = data[CATCH > 0][order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]
  data = data[order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]

  data$YEAR    = as.integer(data$YEAR)
  data$QUARTER = as.integer(data$QUARTER)

  data = data[, .(FLAG_COUNTRY_CODE, REP_COUNTRY_CODE, YEAR, QUARTER, IOTC_FISHERY_CODE, IOTC_FISHERY_NAME, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, CATCH, CATCH_UNIT_CODE,
                  SPECIES_CODE, NAME_LT, NAME_EN, IS_IOTC, IS_AGGREGATE, IS_SSI, IS_BAIT, WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE, SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]

  return(
    list(
      META = metadata,
      DATA = data
    )
  )
}

read_form_NC_old = function(filename) {
  all  = as.data.table(read.xlsx(filename, sheet = "NC", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = all[26:nrow(all)]

  form_type    = "1-RC"
  form_version = "Rev18"

  data_columns = c("QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE")
  species_data_columns = all[23, 9:length(colnames(all))]
  colnames(species_data_columns) = NULL
  species_data_columns = unlist(species_data_columns)

  columns = append(data_columns, species_data_columns)

  colnames(data) = columns

  data = check_and_remove_extra_rows(data)

  DFC = 1
  DLC = length(data_columns)
  SFC = DLC + 1
  SLC = length(colnames(data))

  data = data.table(data[, DFC:DLC], sapply(data[, SFC:SLC, with = FALSE], as.numeric))

  data = fix_repeated_species(data, check_repeated_species(colnames(data)))

  fp_name   = all[ 9][[2]]
  fp_mail   = all[10][[2]]

  org_name   = all[ 9][[17]]
  org_mail   = all[10][[17]]

  finalization_date = as.Date(NA)
  submission_date   = as.Date(NA)

  year      = as.integer(   all[13][[19]])
  unit      = sanitize_code(all[14][[19]])
  flag_country = sanitize_code(all[14][[ 3]])
  rep_country  = sanitize_code(all[13][[ 3]])

  comments = all[17][[2]]

  metadata = list(
    VERSION = form_version,
    FOCAL_POINT_NAME = fp_name,
    FOCAL_POINT_MAIL = fp_mail,
    ORGANIZATION_NAME= org_name,
    ORGANIZATION_MAIL= org_mail,
    FINALIZATION_DATE = finalization_date,
    SUBMISSION_DATE = submission_date,
    YEAR = year,
    FLAG_COUNTRY = flag_country,
    REP_COUNTRY  = rep_country,
    CATCH_UNIT = unit,
    COMMENTS = comments
  )

  data = data.table(FLAG_COUNTRY_CODE = flag, REP_COUNTRY_CODE = rep_flag, YEAR = year, data)

  data =
    melt(
      data,
      id.vars       = c("FLAG_COUNTRY_CODE", "REP_COUNTRY_CODE", "YEAR", "QUARTER", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "PRELIMINARY", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "TARGET_SPECIES_CODE", "COVERAGE_CODE"),
      variable.name =   "SPECIES_CODE",
      value.name    =   "CATCH"
    )

  data[, IOTC_FISHERY_CODE   := sanitize_code(IOTC_FISHERY_CODE)]
  data[, FISHING_GROUND_CODE := sanitize_code(FISHING_GROUND_CODE)]
  data[, DATA_SOURCE_CODE    := sanitize_code(DATA_SOURCE_CODE)]
  data[, DATA_PROCESSING_CODE:= sanitize_code(DATA_PROCESSING_CODE)]
  data[, TARGET_SPECIES_CODE := sanitize_code(TARGET_SPECIES_CODE)]
  data[, COVERAGE_CODE       := sanitize_code(COVERAGE_CODE)]
  data[, SPECIES_CODE        := sanitize_code(SPECIES_CODE)]
  data[, CATCH_UNIT_CODE     := unit]
  data[, RETAINED_REASON_CODE:= "RFL"]

  SPECIES   = load_species()
  FISHERIES = load_iotc_fisheries()

  data = merge(data, SPECIES,   by.x = "SPECIES_CODE",      by.y = "CODE", all.x = TRUE)
  data = merge(data, FISHERIES, by.x = "IOTC_FISHERY_CODE", by.y = "IOTC_FISHERY_CODE", all.x = TRUE)[!is.na(CATCH)]

  data$CATCH = round(as.numeric(data$CATCH), 3)

  #data = data[CATCH > 0][order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]
  data = data[order(IOTC_FISHERY_CODE, QUARTER, SPECIES_CODE)]

  data$YEAR    = as.integer(data$YEAR)
  data$QUARTER = as.integer(data$QUARTER)

  data = data[, .(FLAG_COUNTRY_CODE, REP_COUNTRY_CODE, YEAR, QUARTER, IOTC_FISHERY_CODE, IOTC_FISHERY_NAME, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, CATCH, CATCH_UNIT_CODE,
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
         NO_FLAG_COUNTRY= is.na(metadata$FLAG_COUNTRY),
         NO_REP_COUNTRY = is.na(metadata$REP_COUNTRY),
         NO_CATCH_UNIT= is.na(metadata$CATCH_UNIT)
    )
  )
}

analyze_form_NC_data = function(data) {
  data[is.na(CATCH), CATCH := 0]

  SP_unknown  = unique(data[is.na(NAME_LT) & is.na(NAME_EN)]$SPECIES_CODE)
  FI_unknown  = unique(data[!is.na(IOTC_FISHERY_CODE) & is.na(IOTC_FISHERY_NAME)]$IOTC_FISHERY_CODE)

  SP_no_catch = unique(data[, .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE)][is.na(CATCH) | CATCH == 0]$SPECIES_CODE)
  FI_no_catch = unique(data[, .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE)][is.na(CATCH) | CATCH == 0]$IOTC_FISHERY_CODE)

  Q_FI_FG_SP = data[, .(QUARTER, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, SPECIES_CODE)]

  Q_FI_FG_SP_duplicates =
    data[, .(RECORDS = .N), keyby = .(QUARTER, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, SPECIES_CODE)][RECORDS > 1]

  Q_FI_FG_SP_incomplete =
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
        is.na(RETAINED_REASON_CODE) |
        is.na(SPECIES_CODE),
        .(QUARTER, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, PRELIMINARY, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, TARGET_SPECIES_CODE, COVERAGE_CODE, RETAINED_REASON_CODE, SPECIES_CODE)
      ]
    )

  return(
    list(
      TOTAL_CATCHES           = sum(data$CATCH),
      PRELIMINARY_CATCHES     = sum(data[PRELIMINARY == "PRE"]$CATCH),
      FINAL_CATCHES           = sum(data[PRELIMINARY != "PRE"]$CATCH),
      IOTC_CATCHES            = sum(data[IS_IOTC == TRUE ]$CATCH),
      NON_IOTC_CATCHES        = sum(data[IS_IOTC == FALSE]$CATCH),
      UNKNOWN_SPECIES_CATCHES = sum(data[is.na(IS_IOTC)]$CATCH),
      UNKNOWN_FISHERY_CATCHES = sum(data[is.na(IOTC_FISHERY_CODE) | is.na(IOTC_FISHERY_NAME)]$CATCH),
      SPECIES_AGGR_CATCHES    = sum(data[IS_AGGREGATE == TRUE]$CATCH),
      NUM_SPECIES             = length(unique(data[!is.na(SPECIES_CODE)]$SPECIES_CODE)),
      NUM_FISHERIES           = length(unique(data[!is.na(IOTC_FISHERY_CODE)]$IOTC_FISHERY_CODE)),
      UNKNOWN_SPECIES         = SP_unknown,
      SPECIES_WITH_NO_CATCH   = SP_no_catch,
      UNKNOWN_FISHERIES       = FI_unknown,
      FISHERIES_WITH_NO_CATCH = FI_no_catch,
      CATCH_BY_SPECIES        = data[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE, NAME_EN, SPECIES_CATEGORY_CODE, IS_IOTC)],
      CATCH_BY_FISHERY        = data[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE)],
      ALL_STRATA              = Q_FI_FG_SP,
      DUPLICATE_STRATA        = Q_FI_FG_SP_duplicates,
      INCOMPLETE_STRATA       = Q_FI_FG_SP_incomplete
    )
  )
}

#'Analyzes the processed content of a form 1-RC and returns the analysis results for both the METADATA and DATA sections
#'@export
analyze_form_NC = function(processed_form) {
  META = analyze_form_NC_metadata(processed_form$META)
  DATA = analyze_form_NC_data(processed_form$DATA)

  return(append(META, DATA))
}

#'Reads a form 1-RC and returns the analysis results for both the METADATA and DATA sections
#'@export
read_and_analyze_form_NC = function(filename) {
  return(analyze_form_NC(read_form_NC(filename)))
}

#'Exports the results of processing the form 1-RC in the intermediate format required for incorporation in the IOTDB
#'@export
output_IOTDB = function(processed_data, flag_country, rep_country, source = "LO", quality = "FAIR", LLTU = "LL", comment = "No comment set") {
  FLEET_CODE = fleet_code_for(flag_country, rep_country)

  DATA_IOTDB =
    data.table(
      YEAR             = processed_data$YEAR,
      QUARTER          = processed_data$QUARTER,
      IOTC_FISHERY_CODE= processed_data$IOTC_FISHERY_CODE,
      Country          = flag_country,
      ReportingCountry = rep_country,
      Grid             = ifelse(processed_data$FISHING_GROUND_CODE == "IRWESIO", "F51", ifelse(processed_data$FISHING_GROUND_CODE == "IREASIO", "F57", NA)),
      Gear             = processed_data$IOTC_FISHERY_CODE,
      Species          = processed_data$SPECIES_CODE,
      Catch            = processed_data$CATCH,
      CatchUnit        = processed_data$CATCH_UNIT
    )

  GEAR_MAPPINGS = load_gear_mappings()[, .(IOTC_FISHERY_CODE, GEAR_CODE)]

  DATA_IOTDB[ is.na(YEAR), `:=`(TimeIntervalStart = as.Date(NA), TimeIntervalEnd = as.Date(NA))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & ( is.na(QUARTER) | QUARTER == 0 ), TimeIntervalStart := as.Date(paste0(YEAR, "-01-01"))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & ( is.na(QUARTER) | QUARTER == 0 ), TimeIntervalEnd   := as.Date(paste0(YEAR, "-12-31"))]

  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 1, TimeIntervalStart := as.Date(paste0(YEAR, "-01-01"))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 1, TimeIntervalEnd   := as.Date(paste0(YEAR, "-03-31"))]

  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 2, TimeIntervalStart := as.Date(paste0(YEAR, "-04-01"))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 2, TimeIntervalEnd   := as.Date(paste0(YEAR, "-06-30"))]

  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 3, TimeIntervalStart := as.Date(paste0(YEAR, "-07-01"))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 3, TimeIntervalEnd   := as.Date(paste0(YEAR, "-09-30"))]

  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 4, TimeIntervalStart := as.Date(paste0(YEAR, "-10-01"))]
  DATA_IOTDB = DATA_IOTDB[!is.na(YEAR) & QUARTER == 4, TimeIntervalEnd   := as.Date(paste0(YEAR, "-12-31"))]

  DATA_IOTDB = merge(DATA_IOTDB, GEAR_MAPPINGS, by = "IOTC_FISHERY_CODE", all.x = TRUE)

  DATA_IOTDB[IOTC_FISHERY_CODE == "LLTU"]$GEAR_CODE = LLTU
  DATA_IOTDB$IOTC_FISHERY_CODE = NULL
  DATA_IOTDB[is.na(GEAR_CODE)]$GEAR_CODE = "UNCL"

  DATA_IOTDB = DATA_IOTDB[GEAR_CODE %in% c("PSFS", "PSLS"), GEAR_CODE := "PS"]

  DATA_IOTDB$YEAR         = NULL
  DATA_IOTDB$QUARTER      = NULL

  DATA_IOTDB =
    DATA_IOTDB[, .(Country, ReportingCountry, TimeIntervalStart, TimeIntervalEnd, Grid, Gear = GEAR_CODE, Species, Catch, CatchUnit)]

  DATA_IOTDB[, .(Catch = sum(Catch)), keyby = .(Country, ReportingCountry, TimeIntervalStart, TimeIntervalEnd, Grid, Gear, Species, CatchUnit)]

  DATA_IOTDB$Source = source
  DATA_IOTDB$QualityCode = quality
  DATA_IOTDB$EComments = comment
  DATA_IOTDB$FComments = comment

  return(DATA_IOTDB)
}

#'Exports the results of processing the form 1-RC in the format used by the IOTC libraries
#'@export
output_IOTC = function(processed_data, flag_country, rep_country, LLTU = "LL") {
  DATA_IOTC = processed_data

  GEAR_MAPPINGS = load_gear_mappings()[, .(IOTC_FISHERY_CODE, GEAR_CODE)]

  DATA_IOTC$FLAG_COUNTRY_CODE = NULL
  DATA_IOTC$REP_COUNTRY_CODE = NULL

  DATA_IOTC$PRELIMINARY = NULL
  DATA_IOTC$DATA_SOURCE_CODE = NULL
  DATA_IOTC$DATA_PROCESSING_CODE = NULL
  DATA_IOTC$COVERAGE_CODE = NULL
  DATA_IOTC$IOTC_FISHERY_NAME = NULL
  DATA_IOTC$TARGET_SPECIES_CODE = NULL

  DATA_IOTC$FLEET_CODE = fleet_code_for(flag_country, rep_country)
  #DATA_IOTC$FATE_CODE = ifelse(is.na(DATA_IOTC$RETAINED_REASON_CODE), "RFL", DATA_IOTC$RETAINED_REASON_CODE)
  DATA_IOTC$RETAINED_REASON_CODE = NULL

  DATA_IOTC = merge(DATA_IOTC, GEAR_MAPPINGS, by = "IOTC_FISHERY_CODE", all.x = TRUE)

  DATA_IOTC[IOTC_FISHERY_CODE == "LLTU"]$GEAR_CODE = LLTU
  DATA_IOTC$IOTC_FISHERY_CODE = NULL

  DATA_IOTC[is.na(GEAR_CODE)]$GEAR_CODE = "UNCL"

  DATA_IOTC$QUARTER = NULL
  #DATA_IOTC$QUARTER = as.character(DATA_IOTC$QUARTER)
  #DATA_IOTC$QUARTER = "UNCL"

  DATA_IOTC = merge(DATA_IOTC,
                    query("SELECT ACode,
                                  CASE
                                    WHEN LAggCESF = 'Baitboat' THEN 'BB'
                                    WHEN LAggCESF = 'Gillnet' THEN 'GN'
                                    WHEN LAggCESF = 'Longline' THEN 'LL'
                                    WHEN LAggCESF = 'Handline' THEN 'HL'
                                    WHEN LAggCESF = 'Trolling' THEN 'TL'
                                    WHEN LAggCESF = 'Purse Seine' THEN 'PS'
                                    ELSE 'OT'
                                  END AS FISHERY_GROUP_CODE
                                  FROM cdeGears",
                          connection = DB_IOTDB()),
                    by.x = "GEAR_CODE",
                    by.y = "ACode",
                    all.x = TRUE)

  DATA_IOTC = DATA_IOTC[GEAR_CODE %in% c("PSFS", "PSLS"), GEAR_CODE := "PS"]

  DATA_IOTC = decorate(DATA_IOTC, factorize = TRUE, remove_non_standard_columns = TRUE)

  DATA_IOTC = DATA_IOTC[, .(CATCH = sum(CATCH)), keyby = setdiff(names(DATA_IOTC), "CATCH")]

  return(DATA_IOTC)
}
