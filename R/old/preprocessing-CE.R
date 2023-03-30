is_form_3_CE_temp = function(filename) {
  tryCatch({
    as.data.table(read.xlsx(filename, sheet = "Metadata", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

    return(TRUE)
  }, error = function(e) { return(FALSE) })
}

is_form_3_CE_Rev20 = function(filename) {
  tryCatch({
    DATA = as.data.table(read.xlsx(filename, sheet = "CE_GRID", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

    FORM = str_to_upper(DATA[2][[10]])

    return(!is.na(FORM) & ( FORM == "IOTC FORM 3: CATCH AND EFFORT" | FORM == "Formulaire CTOI 3:Captures et Effort"))
  }, error = function(e) { return(FALSE) })
}

is_form_3_AR_Rev20 = function(filename) {
  tryCatch({
    DATA = as.data.table(read.xlsx(filename, sheet = "CE_GRID", detectDates = TRUE, skipEmptyRows = FALSE, skipEmptyCols = FALSE))

    FORM = str_to_upper(DATA[2][[6]])

    return(!is.na(FORM) & ( FORM == "IOTC FORM 1: NOMINAL CATCH" | FORM == "Formulaire CTOI 3:Captures et Effort"))
  }, error = function(e) { return(FALSE) })
}

#'Reads a form 3-CE and returns the metadata and data contained within
#'@export
read_form_CE = function(filename) {
  if(is.na(filename) | filename == "") return(list(META = NA, DATA = NA))

  if     (is_form_3_CE_temp(filename))  return(read_form_3_CE_temp(filename))
  else if(is_form_3_CE_Rev20(filename)) return(read_form_3_CE_Rev20(filename))
  else if(is_form_3_AR_Rev20(filename)) return(read_form_3_AR_Rev20(filename))

  stop("Unable to process input file: format is not recognized")
}

read_form_3_CE_temp = function(filename) {
  meta = as.data.table(read.xlsx(filename, sheet = "Metadata",               skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = as.data.table(read.xlsx(filename, sheet = "Data",     startRow = 2, skipEmptyRows = TRUE,  skipEmptyCols = TRUE))

  #Removes the "empty" column with "Species code / catch unit" as header
  data[[6]] = NULL

  data_columns = c("MONTH", "FISHING_GROUND_CODE", "ESTIMATION_TYPE_CODE", "EFFORT", "ALTERNATE_EFFORT")
  species_data_columns = colnames(data)[c(6:length(colnames(data)))]

  columns = append(data_columns, species_data_columns)

  colnames(data) = columns

  MT = which(data[1,] == "MT")
  KG = which(data[1,] == "KG")
  NO = which(data[1,] == "NO")
  NN = which(is.na(data[1, ]))

  species_MT = colnames(data)[MT]
  species_KG = colnames(data)[KG]
  species_NO = colnames(data)[NO]
  species_NN = colnames(data)[NN]

  data = data[2:nrow(data)]

  if(length(MT) > 0) setnames(data, MT, paste0(species_MT, "_", "MT"))
  if(length(KG) > 0) setnames(data, KG, paste0(species_KG, "_", "KG"))
  if(length(NO) > 0) setnames(data, NO, paste0(species_NO, "_", "NO"))

  data = check_and_remove_extra_rows(data)

  data = data.table(data[, 1:length(data_columns)], sapply(data[, 1 + length(data_columns):length(colnames(data)), with = FALSE], as.numeric))

  data = fix_repeated_species(data, check_repeated_species(colnames(data)))

  flag_country = sanitize_code(meta[16][[4]])
  rep_country  = sanitize_code(meta[17][[4]])

  year         = as.integer(   meta[19][[4]])

  fishery         = sanitize_code(meta[16][[7]])
  target_species  = sanitize_code(meta[17][[7]])

  preliminary = as.logical(meta[24][[4]])
  raised      = as.logical(meta[25][[4]])

  data_processing = sanitize_code(meta[27][[4]])
  data_source     = sanitize_code(meta[28][[4]])
  coverage        = sanitize_code(meta[29][[4]])

  effort_unit     = sanitize_code(meta[24][[7]])
  effort_unit_alt = sanitize_code(meta[25][[7]])

  metadata = list(
    YEAR = year,
    FLAG_COUNTRY = ifelse(is.null(flag_country), NA, flag_country),
    REP_COUNTRY  = ifelse(is.null(rep_country),  NA, rep_country),
    FISHERY = fishery,
    PRELIMINARY = preliminary,
    RAISED = raised,
    DATA_PROCESSING = data_processing,
    COVERAGE = coverage,
    TARGET_SPECIES = target_species,
    DATA_SOURCE = data_source,
    EFFORT_UNIT = effort_unit,
    EFFORT_UNIT_ALT = effort_unit_alt
  )

  return(
    common_data_processing(
      metadata,
      data
    )
  )
}

read_form_3_CE_Rev20 = function(filename) {
  all = as.data.table(read.xlsx(filename, sheet = "CE_GRID", skipEmptyRows = FALSE, skipEmptyCols = FALSE))
  data = all[29:nrow(all)]

  form_type    = "3-CE"
  form_version = "Rev20"

  #Removes the various columns that will determine the grid...
  data[[2]] = NULL
  data[[2]] = NULL
  data[[2]] = NULL
  data[[2]] = NULL

  data_columns = c("MONTH", "FISHING_GROUND_CODE", "ESTIMATION_TYPE_CODE", "EFFORT", "ALTERNATE_EFFORT")
  species_data_columns = all[25, 10:length(colnames(all))]

  colnames(species_data_columns) = NULL

  species_data_columns = unlist(species_data_columns)

  columns = append(data_columns, species_data_columns)

  colnames(data) = columns

  #data = check_and_remove_extra_rows(data)

  MT = which(all[28,] == "MT")
  KG = which(all[28,] == "KG")
  NO = which(all[28,] == "NO")
  NN = which(is.na(all[28,]))
  NN = NN[NN > 10]

  species_MT = colnames(data)[MT - 4]
  species_KG = colnames(data)[KG - 4]
  species_NO = colnames(data)[NO - 4]
  species_NN = colnames(data)[NN - 4]

  if(length(MT) > 0) setnames(data, MT - 4, paste0(species_MT, "_", "MT"))
  if(length(KG) > 0) setnames(data, KG - 4, paste0(species_KG, "_", "KG"))
  if(length(NO) > 0) setnames(data, NO - 4, paste0(species_NO, "_", "NO"))

  data = check_and_remove_extra_rows(data)

  data = data.table(data[, 1:length(data_columns)], sapply(data[, (1 + length(data_columns)):length(colnames(data)), with = FALSE], as.numeric))

  data = fix_repeated_species(data, check_repeated_species(colnames(data)))

  fp_name   = all[ 9][[2]]
  fp_mail   = all[10][[2]]

  org_name   = all[ 9][[16]]
  org_mail   = all[10][[16]]

  finalization_date = as.Date(NA)
  submission_date   = as.Date(NA)

  flag_country = sanitize_code(all[16][[3]])
  rep_country  = sanitize_code(all[15][[3]])

  year         = as.integer(   all[17][[3]])

  fishery         = sanitize_code(all[17][[25]])
  target_species  = sanitize_code(all[17][[15]])

  preliminary = as.logical(all[17][[5]])
  raised      = as.logical(all[17][[6]])

  data_processing = sanitize_code(all[15][[15]])
  data_source     = sanitize_code(all[16][[26]])
  coverage        = sanitize_code(all[16][[15]])

  effort_unit     = sanitize_code(all[17][[34]])
  effort_unit_alt = sanitize_code(all[18][[34]])

  comments = all[21][[2]]

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
    FISHERY = fishery,
    PRELIMINARY = preliminary,
    RAISED = raised,
    DATA_PROCESSING = data_processing,
    COVERAGE = coverage,
    TARGET_SPECIES = target_species,
    DATA_SOURCE = data_source,
    EFFORT_UNIT = effort_unit,
    EFFORT_UNIT_ALT = effort_unit_alt,
    COMMENTS = comments
  )

  return(
    common_data_processing(
      metadata,
      data
    )
  )
}

common_data_processing = function(metadata, data) {
  year        = metadata$YEAR
  fishery     = metadata$FISHERY
  school_type = ifelse(fishery == "PSFS", "FS", ifelse(fishery == "PSLS", "LS", "UNCL"))

  flag_country = metadata$FLAG_COUNTRY
  rep_country  = metadata$REP_COUNTRY

  if(is.na(flag_country)) fleet_code = rep_country
  else if(is.na(rep_country)) fleet_code = rep_country
  else fleet_code = ifelse(flag_country == rep_country, flag_country, paste0(flag_country, "-", rep_country))

  data =
    data.table(
      data,
      FLEET_CODE                 = fleet_code,
      YEAR                       = year,
      IOTC_FISHERY_CODE          = fishery,
      SCHOOL_TYPE_CODE           = school_type
    )

  data$FISHING_GROUND_CODE = as.character(data$FISHING_GROUND_CODE)
  data$YEAR  = as.integer(data$YEAR)
  data$MONTH = as.integer(data$MONTH)

  data_effort = data[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE, EFFORT, ALTERNATE_EFFORT)]

  data_effort$EFFORT           = as.numeric(data_effort$EFFORT)
  data_effort$ALTERNATE_EFFORT = as.numeric(data_effort$ALTERNATE_EFFORT)

  data_strata = data_effort[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE)]

  data_effort = decorate_EF(data_effort, metadata)

  data_catch = data
  data_catch$EFFORT           = NULL
  data_catch$ALTERNATE_EFFORT = NULL

  data_catch =
    as.data.table(
      melt(
        data_catch,
        id.vars       = c("FLEET_CODE", "YEAR", "MONTH", "IOTC_FISHERY_CODE", "SCHOOL_TYPE_CODE", "FISHING_GROUND_CODE", "ESTIMATION_TYPE_CODE"),
        variable.name = "SPECIES_CODE",
        value.name    = "CATCH"
      )
    )

  data_catch$CATCH_UNIT_CODE = NA
  data_catch$CATCH_UNIT_CODE = as.character(data_catch$CATCH_UNIT_CODE) #Otherwise it will assume CATCH_UNIT is a logical column...

  data_catch[!is.na(SPECIES_CODE) & SPECIES_CODE %like% "_MT$", CATCH_UNIT_CODE := "MT"]
  data_catch[!is.na(SPECIES_CODE) & SPECIES_CODE %like% "_KG$", CATCH_UNIT_CODE := "KG"]
  data_catch[!is.na(SPECIES_CODE) & SPECIES_CODE %like% "_NO$", CATCH_UNIT_CODE := "NO"]
  data_catch[!is.na(SPECIES_CODE), SPECIES_CODE := str_replace(SPECIES_CODE, "(\\_[A-Z]+)$", "")]

  data_catch[, SPECIES_CODE := sanitize_code(SPECIES_CODE)]
  data_catch[, CATCH := round(as.numeric(CATCH), 3)]
  data_catch = decorate_CE(data_catch, metadata)

  data_strata_u = unique(data_strata)
  data_effort_u = data_effort[, `:=`(EFFORT = sum(EFFORT, na.rm = TRUE), ALTERNATE_EFFORT = sum(ALTERNATE_EFFORT, na.rm = TRUE)),
                              keyby = .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE, EFFORT_UNIT_CODE, ALTERNATE_EFFORT_UNIT_CODE)]

  data_catch_u = data_catch[, .(CATCH = sum(CATCH, na.rm = TRUE)),
                            keyby = .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE, SPECIES_CODE, CATCH_UNIT_CODE)]
  data_catch_u = decorate_CE(data_catch_u, metadata)

  SPECIES   = load_species()

  SPECIES$IOTDB_CODE = NULL
  SPECIES = unique(SPECIES)

  data_catch   = merge(data_catch,   SPECIES, by.x = "SPECIES_CODE", by.y = "IOTC_CODE", all.x = TRUE, allow.cartesian = TRUE)
  data_catch_u = merge(data_catch_u, SPECIES, by.x = "SPECIES_CODE", by.y = "IOTC_CODE", all.x = TRUE, allow.cartesian = TRUE)

  data_catch   = data_catch[!is.na(CATCH) & CATCH > 0]
  data_catch_u = data_catch_u[!is.na(CATCH) & CATCH > 0]

  data_ce      = merge(data_effort_u, data_catch_u,
                       by = c("FLEET_CODE", "YEAR", "MONTH", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "SCHOOL_TYPE_CODE", "ESTIMATION_TYPE_CODE",
                              "PRELIMINARY", "RAISED", "DATA_PROCESSING_CODE", "COVERAGE_CODE", "TARGET_SPECIES_CODE", "DATA_SOURCE_CODE"),
                       all.x = TRUE,
                       allow.cartesian = TRUE)

  data_ce = data_ce[order(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, SPECIES_CODE)]

  data_effort   = update_EF(data_effort)
  data_effort_u = update_EF(data_effort_u)
  data_ce       = update_EF(data_ce[!SPECIES_CODE %like% "^X"]) # Removes all rows corresponding to columns without species

  return(
    list(
      META = metadata,
      DATA = list(
        STRATA  = data_strata,   # All strata
        STRATA_U= data_strata_u, # Unique strata
        EFFORT  = rearrange_EF(data_effort),   # All effort records
        EFFORT_U= rearrange_EF(data_effort_u), # Unique effort records (with effort and alternate effort summed for all duplicate strata)
        CATCH   = rearrange_CA(data_catch),    # All catch records
        CATCH_U = rearrange_CA(data_catch_u),  # Unique catch records (with catches summed for all duplicate strata)
        DATA_CE = rearrange_CE(data_ce)        # All catch-and-effort records
      )
    )
  )
}

#'Analyzes the processed content of a form 3-CE and returns the analysis results for both the METADATA and DATA sections
#'@export
analyze_form_CE = function(processed_form) {
  META = analyze_form_CE_metadata(processed_form$META)
  DATA = analyze_form_CE_data(processed_form$DATA)

  return(append(META, DATA))
}

#'Reads a form 1-RC and returns the analysis results for both the METADATA and DATA sections
#'@export
read_and_analyze_form_CE = function(filename) {
  return(analyze_form_CE(read_form_CE(filename)))
}

analyze_form_CE_metadata = function(metadata) {
  return(
    list(NO_YEAR                  = is.na(metadata$YEAR),
         NO_FLAG_COUNTRY          = is.na(metadata$FLAG_COUNTRY),
         NO_REP_COUNTRY           = is.na(metadata$REP_COUNTRY),
         NO_FISHERY               = is.na(metadata$FISHERY),
         NO_DATA_PROCESSING       = is.na(metadata$DATA_PROCESSING),
         NO_COVERAGE              = is.na(metadata$COVERAGE),
         NO_TARGET_SPECIES        = is.na(metadata$TARGET_SPECIES),
         NO_DATA_SOURCE           = is.na(metadata$DATA_SOURCE),
         NO_EFFORT_UNIT           = is.na(metadata$EFFORT_UNIT),
         NO_ALTERNATE_EFFORT_UNIT = is.na(metadata$EFFORT_UNIT_ALT),
         SAME_EFFORT_UNITS        = !is.na(metadata$EFFORT_UNIT) &
                                    !is.na(metadata$EFFORT_UNIT_ALT) &
                                    metadata$EFFORT_UNIT == metadata$EFFORT_UNIT_ALT
    )
  )
}

catch_analysis = function(data_CE) {
  SP_unknown  = unique(data_CE[is.na(SPECIES_NAME_LT) & is.na(SPECIES_NAME_EN)]$SPECIES_CODE)
  SP_no_catch = unique(data_CE[, .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE)][is.na(CATCH) | CATCH == 0]$SPECIES_CODE)

  return(
    list(
      TOTAL_CATCHES           = sum(data_CE$CATCH),
      PRELIMINARY_CATCHES     = sum(data_CE[PRELIMINARY == TRUE ]$CATCH),
      FINAL_CATCHES           = sum(data_CE[PRELIMINARY == FALSE]$CATCH),
      RAISED_CATCHES          = sum(data_CE[RAISED == TRUE ]$CATCH),
      UNRAISED_CATCHES        = sum(data_CE[RAISED == FALSE]$CATCH),
      IOTC_CATCHES            = sum(data_CE[IS_IOTC_SPECIES == TRUE ]$CATCH),
      NON_IOTC_CATCHES        = sum(data_CE[IS_IOTC_SPECIES == FALSE]$CATCH),
      UNKNOWN_SPECIES_CATCHES = sum(data_CE[is.na(IS_IOTC_SPECIES)]$CATCH),
      SPECIES_AGGR_CATCHES    = sum(data_CE[IS_AGGREGATE_SPECIES == TRUE]$CATCH),
      NUM_SPECIES             = length(unique(data_CE[!is.na(SPECIES_CODE)]$SPECIES_CODE)),
      NUM_FISHERIES           = length(unique(data_CE[!is.na(IOTC_FISHERY_CODE)]$IOTC_FISHERY_CODE)),
      UNKNOWN_SPECIES         = SP_unknown,
      SPECIES_WITH_NO_CATCH   = SP_no_catch,
      CATCH_BY_SPECIES        = data_CE[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE, SPECIES_NAME_EN, SPECIES_CATEGORY_CODE, IS_IOTC_SPECIES)]
    )
  )
}

analyze_form_CE_data = function(metadata, data_all) {
  data_catch  = data_all$DATA_CE
  data_effort = data_all$EFFORT

  data_catch[is.na(CATCH), CATCH := 0]

  ALL_FGS = load_fishing_grounds()

  FG_num = length(unique(data_effort[!is.na(FISHING_GROUND_CODE)]$FISHING_GROUND_CODE))
  FG_unknown = unique(data_effort[!FISHING_GROUND_CODE %in% ALL_FGS$FISHING_GROUND_CODE]$FISHING_GROUND_CODE)

  M_FI_FG       = data_effort[, .(RECORDS = .N), keyby = .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)]
  M_FI_FG_CU_SP = data_catch [, .(RECORDS = .N), keyby = .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, CATCH_UNIT_CODE, SPECIES_CODE)]

  M_FI_FG_duplicates       = M_FI_FG      [RECORDS > 1]
  M_FI_FG_CU_SP_duplicates = M_FI_FG_CU_SP[RECORDS > 1]

  M_FI_FG_incomplete =
    unique(
      data_effort[
        is.na(MONTH) |
        is.na(IOTC_FISHERY_CODE) |
        is.na(SCHOOL_TYPE_CODE) |
        is.na(FISHING_GROUND_CODE),
        .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)
      ]
    )

  M_FI_FG_CU_SP_incomplete =
    unique(
      data_catch[
        is.na(MONTH) |
        is.na(IOTC_FISHERY_CODE) |
        is.na(SCHOOL_TYPE_CODE) |
        is.na(FISHING_GROUND_CODE) |
        is.na(CATCH_UNIT_CODE) |
        is.na(SPECIES_CODE),
        .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, CATCH_UNIT_CODE, SPECIES_CODE)
      ]
    )

  SP_num = length(unique(data_catch[!is.na(SPECIES_CODE)]$SPECIES_CODE))
  SP_unknown  = unique(data_catch[!is.na(SPECIES_CODE) & is.na(SPECIES_NAME_LT) & is.na(SPECIES_NAME_EN)]$SPECIES_CODE)
  SP_no_catch = unique(data_catch[!is.na(SPECIES_CODE), .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE)][is.na(CATCH) | CATCH == 0]$SPECIES_CODE)

  return(
    list(
      NUM_FISHING_GROUNDS     = FG_num,
      UNKNOWN_FISHING_GROUNDS = FG_unknown,
      NUM_SPECIES             = SP_num,
      UNKNOWN_SPECIES         = SP_unknown,
      SPECIES_WITH_NO_CATCH   = SP_no_catch,
      ALL_EFFORT_STRATA       = M_FI_FG,
      ALL_CATCH_STRATA        = M_FI_FG_CU_SP,
      DUPLICATE_EFFORT_STRATA = M_FI_FG_duplicates,
      INCOMPLETE_EFFORT_STRATA= M_FI_FG_incomplete,
      DUPLICATE_CATCH_STRATA  = M_FI_FG_CU_SP_duplicates,
      INCOMPLETE_CATCH_STRATA = M_FI_FG_CU_SP_incomplete,
      CATCH_ANALYSIS = list(
        MT = catch_analysis(data_catch[CATCH_UNIT_CODE == "MT"]),
        KG = catch_analysis(data_catch[CATCH_UNIT_CODE == "KG"]),
        NO = catch_analysis(data_catch[CATCH_UNIT_CODE == "NO"]),
        NN = catch_analysis(data_catch[is.na(CATCH_UNIT_CODE)])
      )
    )
  )
}

#'Analyzes the processed content of a form 3-CE (intermediate format) and returns the analysis results for both the METADATA and DATA sections
#'@export
analyze_form_CE = function(processed_form) {
  META = analyze_form_CE_metadata(processed_form$META)
  DATA = analyze_form_CE_data(processed_form$META,
                              processed_form$DATA)

  return(append(META, DATA))
}

#'Reads a form 3-CE (intermediate format) and returns the analysis results for both the METADATA and DATA sections
#'@export
read_and_analyze_form_CE = function(filename) {
  return(analyze_form_CE(read_form_CE(filename)))
}

decorate_CE = function(data, metadata) {
  return(
    data.table(
      data,
      PRELIMINARY                = metadata$PRELIMINARY,
      RAISED                     = metadata$RAISED,
      DATA_PROCESSING_CODE       = metadata$DATA_PROCESSING,
      COVERAGE_CODE              = metadata$COVERAGE,
      TARGET_SPECIES_CODE        = metadata$TARGET_SPECIES,
      DATA_SOURCE_CODE           = metadata$DATA_SOURCE
    )
  )
}

decorate_EF = function(data, metadata) {
  data = decorate_CE(data, metadata)

  return(
    data.table(
      data,
      EFFORT_UNIT_CODE           = metadata$EFFORT_UNIT,
      ALTERNATE_EFFORT_UNIT_CODE = metadata$EFFORT_UNIT_ALT
    )
  )
}

update_EF = function(data) {
  data[is.na(ALTERNATE_EFFORT_UNIT_CODE) & ALTERNATE_EFFORT == 0, ALTERNATE_EFFORT := NA]
  data[is.na(EFFORT_UNIT_CODE) & EFFORT == 0, EFFORT := NA]

  return(data)
}

rearrange_EF = function(data) {
 return(
   data[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE,
            PRELIMINARY, RAISED, DATA_PROCESSING_CODE, COVERAGE_CODE, TARGET_SPECIES_CODE, DATA_SOURCE_CODE, EFFORT,
            EFFORT_UNIT_CODE, ALTERNATE_EFFORT, ALTERNATE_EFFORT_UNIT_CODE)]
 )
}

rearrange_CA = function(data) {
  return (
    data[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE,
             PRELIMINARY, RAISED, DATA_PROCESSING_CODE, COVERAGE_CODE, TARGET_SPECIES_CODE, DATA_SOURCE_CODE, EFFORT,
             CATCH, CATCH_UNIT_CODE,
             SPECIES_CODE, SPECIES_NAME_LT = NAME_LT, SPECIES_NAME_EN = NAME_EN,
             IS_IOTC_SPECIES = IS_IOTC, IS_AGGREGATE_SPECIES = IS_AGGREGATE, IS_SSI, IS_BAIT, SPECIES_WP_CODE = WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE,
             SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]
  )
}

rearrange_CE = function(data) {
  return (
    data[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE,
             PRELIMINARY, RAISED, DATA_PROCESSING_CODE, COVERAGE_CODE, TARGET_SPECIES_CODE, DATA_SOURCE_CODE, EFFORT,
             EFFORT_UNIT_CODE, ALTERNATE_EFFORT, ALTERNATE_EFFORT_UNIT_CODE, CATCH, CATCH_UNIT_CODE,
             SPECIES_CODE, SPECIES_NAME_LT = NAME_LT, SPECIES_NAME_EN = NAME_EN,
             IS_IOTC_SPECIES = IS_IOTC, IS_AGGREGATE_SPECIES = IS_AGGREGATE, IS_SSI, IS_BAIT, SPECIES_WP_CODE = WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE,
             SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]
  )
}
