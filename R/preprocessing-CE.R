#'Reads a form 3-CE (intermediate format) and returns the information contained in its METADATA and DATA sections
#'@export
read_form_CE = function(filename) {
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

  fleet     = sanitize_code(meta[16][[4]])
  rep_fleet = sanitize_code(meta[17][[4]])
  year      = as.integer(   meta[19][[4]])

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
    FLEET = fleet,
    REP_FLEET = rep_fleet,
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

  if(is.na(fleet)) fleet_code = rep_fleet
  else if(is.na(rep_fleet)) fleet_code = fleet
  else fleet_code = ifelse(fleet == rep_fleet, fleet, paste0(fleet, "-", rep_fleet))

  data =
    data.table(
      data,
      FLEET_CODE                 = fleet_code,
      YEAR                       = year,
      IOTC_FISHERY_CODE          = fishery,
      SCHOOL_TYPE_CODE           = ifelse(fishery == "PSFS", "FS", ifelse(fishery == "PSLS", "LS", "UNCL"))
    )

  data$FISHING_GROUND_CODE = as.character(data$FISHING_GROUND_CODE)
  data$YEAR  = as.integer(data$YEAR)
  data$MONTH = as.integer(data$MONTH)


  data_effort = data       [, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE, EFFORT, ALTERNATE_EFFORT)]
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

  data_catch   = merge(data_catch,   load_species(), by.x = "SPECIES_CODE", by.y = "CODE", all.x = TRUE)
  data_catch_u = merge(data_catch_u, load_species(), by.x = "SPECIES_CODE", by.y = "CODE", all.x = TRUE)

  data_catch   = data_catch[!is.na(CATCH) & CATCH > 0]
  data_catch_u = data_catch_u[!is.na(CATCH) & CATCH > 0]

  data_ce      = merge(data_effort_u, data_catch_u,
                       by = c("FLEET_CODE", "YEAR", "MONTH", "IOTC_FISHERY_CODE", "FISHING_GROUND_CODE", "SCHOOL_TYPE_CODE", "ESTIMATION_TYPE_CODE",
                              "PRELIMINARY", "RAISED", "DATA_PROCESSING_CODE", "COVERAGE_CODE", "TARGET_SPECIES_CODE", "DATA_SOURCE_CODE"),
                       all.x = TRUE,
                       allow.cartesian = TRUE)

  data_ce = data_ce[order(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, FISHING_GROUND_CODE, SPECIES_CODE)]

  data_effort   = rearrange_EF(update_EF(data_effort))
  data_effort_u = rearrange_EF(update_EF(data_effort_u))
  data_ce       = rearrange_CE(update_EF(data_ce[!SPECIES_CODE %like% "^X"])) # Removes all rows corresponding to columns without species

  return(
    list(
      META = metadata,
      DATA = list(
        STRATA  = data_strata,   # All strata
        STRATA_U= data_strata_u, # Unique strata
        EFFORT  = data_effort,   # All effort records
        EFFORT_U= data_effort_u, # Unique effort records (with effort and alternate effort summed for all duplicate strata)
        CATCH   = data_catch,    # All catch records
        CATCH_U = data_catch_u,  # Unique catch records (with catches summed for all duplicate strata)
        DATA_CE = data_ce        # All catch-and-effort records
      )
    )
  )
}

analyze_form_CE_metadata = function(metadata) {
  return(
    list(NO_YEAR                  = is.na(metadata$YEAR),
         NO_FLEET                 = is.na(metadata$FLEET),
         NO_REP_FLEET             = is.na(metadata$REP_FLEET),
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

analyze_form_CE_data = function(metadata, data_all) {
  data_catch  = data_all$DATA_CE
  data_effort = data_all$EFFORT

  data_catch[is.na(CATCH), CATCH := 0]

  SP_unknown  = unique(data_catch[is.na(NAME_LT) & is.na(NAME_EN)]$SPECIES_CODE)
  FI_unknown  = data_catch[is.na(IOTC_FISHERY_CODE) & !is.na(CATCH) & CATCH > 0]

  SP_no_catch = unique(data_catch[, .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE, CATCH_UNIT_CODE)][is.na(CATCH) | CATCH == 0]$SPECIES_CODE)
  FI_no_catch = unique(data_catch[, .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE, CATCH_UNIT_CODE)][is.na(CATCH) | CATCH == 0]$IOTC_FISHERY_CODE)

  M_FI_FG_duplicates    = data_effort[, .(RECORDS = .N), keyby = .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE)][RECORDS > 1]
  M_FI_FG_SP_duplicates = data_catch [, .(RECORDS = .N), keyby = .(MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, SPECIES_CODE)][RECORDS > 1]

  return(
    list(
      UNKNOWN_SPECIES         = SP_unknown,
      SPECIES_WITH_NO_CATCH   = SP_no_catch,
      UNKNOWN_FISHERIES       = FI_unknown,
      FISHERIES_WITH_NO_CATCH = FI_no_catch,
      DUPLICATE_EFFORT_STRATA = M_FI_FG_duplicates,
      DUPLICATE_CATCH_STRATA  = M_FI_FG_SP_duplicates,
      CATCH_BY_FISHERY        = data_catch[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(IOTC_FISHERY_CODE, CATCH_UNIT_CODE)],
      CATCH_BY_SPECIES        = data_catch[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(SPECIES_CODE, CATCH_UNIT_CODE)],
      TOTAL_CATCHES           = data_catch[!is.na(CATCH), .(CATCH = sum(CATCH)), keyby = .(CATCH_UNIT_CODE)]
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

rearrange_CE = function(data) {
  return (
    data[, .(FLEET_CODE, YEAR, MONTH, IOTC_FISHERY_CODE, SCHOOL_TYPE_CODE, FISHING_GROUND_CODE, ESTIMATION_TYPE_CODE,
             PRELIMINARY, RAISED, DATA_PROCESSING_CODE, COVERAGE_CODE, TARGET_SPECIES_CODE, DATA_SOURCE_CODE, EFFORT,
             EFFORT_UNIT_CODE, ALTERNATE_EFFORT, ALTERNATE_EFFORT_UNIT_CODE, CATCH, CATCH_UNIT_CODE, SPECIES_CODE, NAME_LT, NAME_EN,
             IS_IOTC, IS_AGGREGATE, IS_SSI, IS_BAIT, WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE,
             SPECIES_FAMILY = ASFIS_FAMILY, SPECIES_ORDER = ASFIS_ORDER, IUCN_STATUS_CODE = IUCN_STATUS)]
  )
}
