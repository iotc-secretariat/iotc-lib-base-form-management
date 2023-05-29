#' @include IOTC_form_CESF_multiple_class.R
#' @export IOTCForm3CEMultiple
IOTCForm3CEMultiple = setClass(
  "IOTCForm3CEMultiple",
  contains = "IOTCFormCESFMultiple"
)

setMethod("form_type", "IOTCForm3CEMultiple", function(form) {
  return("3-CE-multiple")
})

setMethod("form_version", "IOTCForm3CEMultiple", function(form) {
  return("1.0.0-legacy")
})

setMethod("form_dataset_code", "IOTCForm3CEMultiple", function(form) {
  return("CE")
})

setMethod("grid_validator", "IOTCForm3CEMultiple", function(form) {
  return(is_grid_AR_valid)
})

setMethod("allow_empty_data", "IOTCForm3CEMultiple", function(form) {
  return(TRUE)
})

setMethod("optional_strata_columns", "IOTCForm3CEMultiple", function(form) {
  return(14:17) # Secondary and tertiary effort codes / values
})

setMethod("first_data_column", "IOTCForm3CEMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "T"))
})

setMethod("first_data_row", "IOTCForm3CEMultiple", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm3CEMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm3CEMultiple", function(form) {
  return(which(EXCEL_COLUMNS == "R"))
})


setMethod("validate_months_multiple", list(form = "IOTCForm3CEMultiple", strata = "data.table"), function(form, strata) {
  start = Sys.time()

  l_info("IOTCForm3CEMultiple.validate_months")

  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_info(paste0("IOTCForm3CEMultiple.validate_months: ", Sys.time() - start))

  return(
    list(
      incomplete_months  = incomplete_months
    )
  )
})

setMethod("extract_data", "IOTCForm3CEMultiple", function(form) {
  l_info("IOTCForm3CEMultiple.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  strata = form_data[4:nrow(form_data)][, c(2:18)]
  colnames(strata) = c("MONTH", "FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "PRIMARY_EFFORT_CODE", "PRIMARY_EFFORT", "SECONDARY_EFFORT_CODE", "SECONDARY_EFFORT", "TERTIARY_EFFORT_CODE", "TERTIARY_EFFORT")

  strata[, MONTH    := as.integer(MONTH)]

  records = form_data[2:nrow(form_data), 20:ncol(form_data)]

  species_codes    = unlist(lapply(records[1], trim), use.names = FALSE)
  catch_unit_codes = unlist(lapply(records[2], trim), use.names = FALSE)

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  records_original = records[3:nrow(records)]
  records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata = strata,
      records =
        list(
          codes = list(
            species     = species_codes,
            catch_units = catch_unit_codes
          ),
          data = list(
            CE_SF_data_original = records_original,
            CE_SF_data          = records
          )
        )
    )
  )
})

setMethod("validate_data", list(form = "IOTCForm3CEMultiple", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start = Sys.time()

  l_info("IOTCForm3CEMultiple.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  l_info(paste0("IOTCForm3CEMultiple.validate_data.super(): ", Sys.time() - start))
  start = Sys.time()

  strata  = form@data$strata

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata[, 1:3]) # Effort values shall not be considered, as some of them (either secondary, or tertiary, or both) might be left all empty

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(MONTH, FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  # If all months are provided and valid, we check that they're also consistent...
  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (I): ", Sys.time() - start))
  start = Sys.time()

  grid_size = function(code) {
    if(is.na(code) || code == "") return("OTHER")
    else if(str_sub(code, 1, 1) == "5") return("1_DEG")
    else if(str_sub(code, 1, 1) == "6") return("5_DEG")
    return("OTHER")
  }

  fishery_category = function(value) {
    if(!is.na(value) && is_fishery_valid(value)) {
      return(fisheries_for(value)$FISHERY_CATEGORY)
    }

    return(NA)
  }

  grid_status    = data.table(FISHERY_CATEGORY_CODE = sapply(strata$FISHERY_CODE, fishery_category),
                              GRID_CODE = strata$GRID_CODE,
                              MISSING   = sapply(strata$GRID_CODE, is.na),
                              VALID     = sapply(strata$GRID_CODE, grid_validator(form)),
                              SIZE      = sapply(strata$GRID_CODE, grid_size))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (I.a): ", Sys.time() - start))
  start = Sys.time()

  grid_status[, WRONG_GRID_TYPE := fifelse(FISHERY_CATEGORY_CODE == "SURFACE",
                                           SIZE != "1_DEG",
                                           fifelse(FISHERY_CATEGORY_CODE == "LONGLINE",
                                                   SIZE == "OTHER",
                                                   FALSE)
                                           )]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (I.b): ", Sys.time() - start))
  start = Sys.time()

  wrong_grid_types = which(grid_status$WRONG_GRID_TYPE == TRUE)

  l_info(paste0("IOTCForm3CEMultiple.validate_data (II.a): ", Sys.time() - start))
  start = Sys.time()

  data_validation_results$strata$checks$main$grids$wrong = list(
    number       = length(wrong_grid_types),
    row_indexes  = wrong_grid_types,
    codes        = strata$GRID_CODE[wrong_grid_types],
    codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
  )

  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
  duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
  duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
  unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (II.b): ", Sys.time() - start))
  start = Sys.time()

  data_validation_results$strata$duplicate =
    list(
      number = length(duplicate_strata),
      row_indexes = spreadsheet_rows_for(form, duplicate_strata)
    )

  data_validation_results$strata$unique =
    list(
      number = length(unique_strata),
      row_indexes = spreadsheet_rows_for(form, unique_strata)
    )

  is_effort_valid = function(value) { return(!is.na(value) & is_numeric(value) & value > 0) } #{ return(!is.na(value) | value > 0) }

  l_info(paste0("IOTCForm3CEMultiple.validate_data (II.c): ", Sys.time() - start))
  start = Sys.time()

  ### Primary effort code + value

  missing_primary_effort_codes = which( sapply(strata$PRIMARY_EFFORT_CODE, is.na))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (II.d): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_effort_codes = which(!sapply(strata$PRIMARY_EFFORT_CODE, is_effort_unit_valid))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (II.e): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_effort_codes = invalid_primary_effort_codes[ ! invalid_primary_effort_codes %in% missing_primary_effort_codes ]
  missing_primary_effort_codes = missing_primary_effort_codes[ ! missing_primary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (III): ", Sys.time() - start))
  start = Sys.time()

  missing_primary_efforts = which( sapply(strata$PRIMARY_EFFORT, is.na))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (III.a): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_efforts = which(!sapply(strata$PRIMARY_EFFORT, is_effort_valid))

  l_info(paste0("IOTCForm3CEMultiple.validate_data (III.b): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_efforts = invalid_primary_efforts[ ! invalid_primary_efforts %in% missing_primary_efforts ]
  missing_primary_efforts = missing_primary_efforts[ ! missing_primary_efforts %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (IV): ", Sys.time() - start))
  start = Sys.time()

  ### Secondary effort code + value

  missing_secondary_effort_codes = which( sapply(strata$SECONDARY_EFFORT_CODE, is.na))
  invalid_secondary_effort_codes = which(!sapply(strata$SECONDARY_EFFORT_CODE, is_effort_unit_valid))
  invalid_secondary_effort_codes = invalid_secondary_effort_codes[ ! invalid_secondary_effort_codes %in% missing_secondary_effort_codes ]
  missing_secondary_effort_codes = missing_secondary_effort_codes[ ! missing_secondary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (V): ", Sys.time() - start))
  start = Sys.time()

  missing_secondary_efforts = which( sapply(strata$SECONDARY_EFFORT, is.na))
  invalid_secondary_efforts = which(!sapply(strata$SECONDARY_EFFORT, is_effort_valid))
  invalid_secondary_efforts = invalid_secondary_efforts[ ! invalid_secondary_efforts %in% missing_secondary_efforts ]
  missing_secondary_efforts = missing_secondary_efforts[ ! missing_secondary_efforts %in% strata_empty_rows ]

  missing_secondary_efforts_      = missing_secondary_efforts     [which(!missing_secondary_efforts      %in% missing_secondary_effort_codes)]
  missing_secondary_effort_codes_ = missing_secondary_effort_codes[which(!missing_secondary_effort_codes %in% missing_secondary_efforts)]

  missing_secondary_efforts      = missing_secondary_efforts_
  missing_secondary_effort_codes = missing_secondary_effort_codes_

  #secondary_efforts_provided= length(which(!sapply(strata$SECONDARY_EFFORT, is.na))) > 0

  l_info(paste0("IOTCForm3CEMultiple.validate_data (VI): ", Sys.time() - start))
  start = Sys.time()

  ### Tertiary effort code + value

  missing_tertiary_effort_codes = which( sapply(strata$TERTIARY_EFFORT_CODE, is.na))
  invalid_tertiary_effort_codes = which(!sapply(strata$TERTIARY_EFFORT_CODE, is_effort_unit_valid))
  invalid_tertiary_effort_codes = invalid_tertiary_effort_codes[ ! invalid_tertiary_effort_codes %in% missing_tertiary_effort_codes ]
  missing_tertiary_effort_codes = missing_tertiary_effort_codes[ ! missing_tertiary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (VII): ", Sys.time() - start))
  start = Sys.time()

  missing_tertiary_efforts = which( sapply(strata$TERTIARY_EFFORT, is.na))
  invalid_tertiary_efforts = which(!sapply(strata$TERTIARY_EFFORT, is_effort_valid))
  invalid_tertiary_efforts = invalid_tertiary_efforts[ ! invalid_tertiary_efforts %in% missing_tertiary_efforts ]
  missing_tertiary_efforts = missing_tertiary_efforts[ ! missing_tertiary_efforts %in% strata_empty_rows ]

  missing_tertiary_efforts_      = missing_tertiary_efforts     [which(!missing_tertiary_efforts      %in% missing_tertiary_effort_codes)]
  missing_tertiary_effort_codes_ = missing_tertiary_effort_codes[which(!missing_tertiary_effort_codes %in% missing_tertiary_efforts)]

  missing_tertiary_efforts      = missing_tertiary_efforts_
  missing_tertiary_effort_codes = missing_tertiary_effort_codes_

  same_effort_unit_ps = which(strata[!is.na(PRIMARY_EFFORT_CODE),   SAME_EFFORT_UNITS_PS := PRIMARY_EFFORT_CODE   == SECONDARY_EFFORT_CODE]$SAME_EFFORT_UNITS_PS == TRUE)
  same_effort_unit_pt = which(strata[!is.na(PRIMARY_EFFORT_CODE),   SAME_EFFORT_UNITS_PT := PRIMARY_EFFORT_CODE   == TERTIARY_EFFORT_CODE ]$SAME_EFFORT_UNITS_PT == TRUE)
  same_effort_unit_st = which(strata[!is.na(SECONDARY_EFFORT_CODE), SAME_EFFORT_UNITS_ST := SECONDARY_EFFORT_CODE == TERTIARY_EFFORT_CODE ]$SAME_EFFORT_UNITS_ST == TRUE)

  #tertiary_efforts_provided= length(which(!sapply(strata$TERTIARY_EFFORT, is.na))) > 0

  l_info(paste0("IOTCForm3CEMultiple.validate_data (VIII): ", Sys.time() - start))
  start = Sys.time()

  data_validation_results$strata$checks$efforts = list(
    same_unit = list(
      primary_secondary = list(
        number = length(same_effort_unit_ps),
        row_indexes = spreadsheet_rows_for(form, same_effort_unit_ps)
      ),
      primary_tertiary = list(
        number = length(same_effort_unit_pt),
        row_indexes = spreadsheet_rows_for(form, same_effort_unit_pt)
      ),
      secondary_tertiary = list(
        number = length(same_effort_unit_st),
        row_indexes = spreadsheet_rows_for(form, same_effort_unit_st)
      )
    ),
    primary = list(
      code = list(
        missing = list(
          number      = length(missing_primary_effort_codes),
          row_indexes = spreadsheet_rows_for(form, missing_primary_effort_codes)
        ),
        invalid = list(
          number        = length(invalid_primary_effort_codes),
          row_indexes   = spreadsheet_rows_for(form, invalid_primary_effort_codes),
          values        = strata$PRIMARY_EFFORT_CODE[invalid_primary_effort_codes],
          values_unique = unique(strata$PRIMARY_EFFORT_CODE[invalid_primary_effort_codes])
        )
      ),
      value = list(
        missing = list(
          number      = length(missing_primary_efforts),
          row_indexes = spreadsheet_rows_for(form, missing_primary_efforts)
        ),
        invalid = list(
          number        = length(invalid_primary_efforts),
          row_indexes   = spreadsheet_rows_for(form, invalid_primary_efforts),
          values        = strata$PRIMARY_EFFORT[invalid_primary_efforts],
          values_unique = unique(strata$PRIMARY_EFFORT[invalid_primary_efforts])
        )
      )
    ),
    secondary = list(
      code = list(
        missing = list(
          number      = length(missing_secondary_effort_codes),
          row_indexes = spreadsheet_rows_for(form, missing_secondary_effort_codes)
        ),
        invalid = list(
          number        = length(invalid_secondary_effort_codes),
          row_indexes   = spreadsheet_rows_for(form, invalid_secondary_effort_codes),
          values        = strata$SECONDARY_EFFORT_CODE[invalid_secondary_effort_codes],
          values_unique = unique(strata$SECONDARY_EFFORT_CODE[invalid_secondary_effort_codes])
        )
      ),
      value = list(
        missing = list(
          number      = length(missing_secondary_efforts),
          row_indexes = spreadsheet_rows_for(form, missing_secondary_efforts)
        ),
        invalid = list(
          number        = length(invalid_secondary_efforts),
          row_indexes   = spreadsheet_rows_for(form, invalid_secondary_efforts),
          values        = strata$SECONDARY_EFFORT[invalid_secondary_efforts],
          values_unique = unique(strata$SECONDARY_EFFORT[invalid_secondary_efforts])
        )
      )
    ),
    tertiary = list(
      code = list(
        missing = list(
          number      = length(missing_tertiary_effort_codes),
          row_indexes = spreadsheet_rows_for(form, missing_tertiary_effort_codes)
        ),
        invalid = list(
          number        = length(invalid_tertiary_effort_codes),
          row_indexes   = spreadsheet_rows_for(form, invalid_tertiary_effort_codes),
          values        = strata$TERTIARY_EFFORT_CODE[invalid_tertiary_effort_codes],
          values_unique = unique(strata$TERTIARY_EFFORT_CODE[invalid_tertiary_effort_codes])
        )
      ),
      value = list(
        missing = list(
          number      = length(missing_tertiary_efforts),
          row_indexes = spreadsheet_rows_for(form, missing_tertiary_efforts)
        ),
        invalid = list(
          number        = length(invalid_tertiary_efforts),
          row_indexes   = spreadsheet_rows_for(form, invalid_tertiary_efforts),
          values        = strata$TERTIARY_EFFORT[invalid_tertiary_efforts],
          values_unique = unique(strata$TERTIARY_EFFORT[invalid_tertiary_efforts])
        )
      )
    )
  )

  records = form@data$records

  catch_data_original = records$data$CE_SF_data_original
  catch_data          = records$data$CE_SF_data

  l_info(paste0("IOTCForm3CEMultiple.validate_data (IX): ", Sys.time() - start))
  start = Sys.time()

  ### Maybe a check shall be added that for surface / coastal fisheries catches shall be necessarily provided in weight?

  missing_species    = which( sapply(records$codes$species, is.na))
  invalid_species    = which(!sapply(records$codes$species, is_species_valid))
  invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (X): ", Sys.time() - start))
  start = Sys.time()

  species_aggregates =
    which(
      unlist(
        sapply(
          records$codes$species,
          function(value) {
            return(
              !is.na(value) && is_species_aggregate(value)
            )
          },
          USE.NAMES = FALSE
        ),
        use.names = FALSE
      )
    )

  l_info(paste0("IOTCForm3CEMultiple.validate_data (XI): ", Sys.time() - start))
  start = Sys.time()

  missing_catch_units = which( sapply(records$codes$catch_units, is.na))
  invalid_catch_units = which(!sapply(records$codes$catch_units, is_catch_unit_valid))
  invalid_catch_units = invalid_catch_units[ ! invalid_catch_units %in% missing_catch_units ]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (XII): ", Sys.time() - start))
  start = Sys.time()

  ### FOLLOWING CHECK HAS TO BE COPIED FROM 1-DI...

  max_length = max(length(records$codes$species),
                   length(records$codes$catch_units))

  species     = pad(records$codes$species,     max_length)
  catch_units = pad(records$codes$catch_units, max_length)

  data_stratification = paste(species, catch_units, sep = "-")

  data_stratification_occurrences = as.data.table(table(data_stratification))

  if(nrow(data_stratification_occurrences) > 0) {
    colnames(data_stratification_occurrences) = c("STRATIFICATION_CODE", "NUM_OCCURRENCES")

    data_stratification_occurrences_multiple = data_stratification_occurrences[NUM_OCCURRENCES > 1]
    data_stratifications_multiple = which(data_stratification %in% data_stratification_occurrences_multiple$STRATIFICATION_CODE)
  } else {
    data_stratification_occurrences_multiple = data.table(STRATIFICATION_CODE = character(), NUM_OCCURRENCES = integer())
    data_stratifications_multiple = as.integer(array())
  }

  l_info(paste0("IOTCForm3CEMultiple.validate_data (XIII): ", Sys.time() - start))
  start = Sys.time()

  numeric_catch_data =
    catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

  l_info(paste0("IOTCForm3CEMultiple.validate_data (XIV): ", Sys.time() - start))
  start = Sys.time()

  non_num_catches  = which(numeric_catch_data == FALSE, arr.ind = TRUE) #sum(numeric_catch_data == FALSE, na.rm = TRUE)

  na_catches       = which(numeric_catch_data == TRUE & is.na(catch_data), arr.ind = TRUE) #sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
  zero_catches     = which(numeric_catch_data == TRUE & catch_data == 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
  negative_catches = which(numeric_catch_data == TRUE & catch_data  < 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
  positive_catches = which(numeric_catch_data == TRUE & catch_data  > 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

  l_info(paste0("IOTCForm3CEMultiple.validate_data (XV): ", Sys.time() - start))
  start = Sys.time()

  data_validation_results$records$checks = list(
    stratifications = list(
      multiple = list(
        number       = length(data_stratifications_multiple),
        col_indexes  = spreadsheet_cols_for(form, data_stratifications_multiple),
        codes_unique = data_stratification_occurrences_multiple$STRATIFICATION_CODE
      )
    ),
    species = list(
      missing = list(
        number      = length(missing_species),
        col_indexes = spreadsheet_cols_for(form, missing_species)
      ),
      invalid = list(
        number       = length(invalid_species),
        col_indexes  = spreadsheet_cols_for(form, invalid_species),
        codes        = records$codes$species[invalid_species],
        codes_unique = unique(records$codes$species[invalid_species])
      ),
      aggregates = list(
        number      = length(species_aggregates),
        col_indexes = spreadsheet_cols_for(form, species_aggregates),
        codes       = records$codes$species[species_aggregates]
      )
    ),
    catch_units = list(
      missing = list(
        number      = length(missing_catch_units),
        col_indexes = spreadsheet_cols_for(form, missing_catch_units)
      ),
      invalid = list(
        number       = length(invalid_catch_units),
        col_indexes  = spreadsheet_cols_for(form, invalid_catch_units),
        codes        = records$codes$catch_units[invalid_catch_units],
        codes_unique = unique(records$codes$catch_units[invalid_catch_units])
      )
    ),
    catch_values = list(
      na = list(
        number = nrow(na_catches),
        cells  = coordinates_to_cells(form, na_catches)
      ),
      zero = list(
        number = nrow(zero_catches),
        cells  = coordinates_to_cells(form, zero_catches)
      ),
      positive = list(
        number = nrow(positive_catches),
        cells  = coordinates_to_cells(form, positive_catches)
      ),
      negative = list(
        number = nrow(negative_catches),
        cells  = coordinates_to_cells(form, negative_catches)
      ),
      non_num  = list(
        number = nrow(non_num_catches),
        cells  = coordinates_to_cells(form, non_num_catches)
      )
    )
  )

  l_info(paste0("IOTCForm3CEMultiple.validate_data: ", Sys.time() - start))

  return(data_validation_results)
})

setMethod("data_validation_summary", list(form = "IOTCForm3CEMultiple", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  l_info("IOTCForm3CEMultiple.data_validation_summary")

  validation_messages = common_data_validation_summary(form,
                                                       metadata_validation_results,
                                                       data_validation_results)

  ### STRATA AND RECORDS

  # This is only true for 3CE / surface

  fishery_info        = metadata_validation_results$general_information$fishery

  ### TO BE RECONSIDERED

  if(FALSE) {
    data_specifications = metadata_validation_results$data_specifications

    if(fishery_info$category == "SURFACE") {
      validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "The provided fishery belongs to the 'surface' category and therefore catches are assumed to be reported in live-weight equivalent and raised to totals by default"))
    }

    if(data_specifications$catch_unit$code == "NO" && fishery_info$category != "LONGLINE") {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = "Catches shall be provided in weight (either kg or t)"))
    }
  }

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  # Validation comes from the superclass

  # Strata checks

  ## Main strata

  if(strata$checks$main$grids$invalid$number > 0) {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid code in row(s) #", paste0(strata$checks$main$grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid grid codes for this dataset")))
  }

  if(strata$checks$main$grids$wrong$number > 0) {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(strata$checks$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery: see row(s) #", paste0(strata$checks$main$grids$wrong$row_indexes, collapse = ", "))))
  }

  # Commented out as the message is handled at superclass level
  #if(strata$duplicate$number > 0)
  #  validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

  ## Efforts

  checks_strata_efforts = checks_strata$efforts

  effort_primary   = checks_strata_efforts$primary

  if(effort_primary$code$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing primary effort code in row(s) #", paste0(effort_primary$code$missing$row_indexes, collapse = ", "))))

  if(effort_primary$code$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid primary effort code in row(s) #", paste0(effort_primary$code$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("fishery", "effortUnits"), " for a list of valid effort unit codes")))

  if(effort_primary$value$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing primary effort value in row(s) #", paste0(effort_primary$value$missing$row_indexes, collapse = ", "))))

  if(effort_primary$value$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid primary effort value in row(s) #", paste0(effort_primary$value$invalid$row_indexes, collapse = ", "))))

  effort_secondary = checks_strata_efforts$secondary

  if(effort_secondary$code$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing secondary effort code in row(s) #", paste0(effort_secondary$code$missing$row_indexes, collapse = ", "))))

  if(effort_secondary$code$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid secondary effort code in row(s) #", paste0(effort_secondary$code$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("fishery", "effortUnits"), " for a list of valid effort unit codes")))

  if(effort_secondary$value$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing secondary effort value in row(s) #", paste0(effort_secondary$value$missing$row_indexes, collapse = ", "))))

  if(effort_secondary$value$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid secondary effort value in row(s) #", paste0(effort_secondary$value$invalid$row_indexes, collapse = ", "))))

  effort_tertiary  = checks_strata_efforts$tertiary

  if(effort_tertiary$code$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing tertiary effort code in row(s) #", paste0(effort_tertiary$code$missing$row_indexes, collapse = ", "))))

  if(effort_tertiary$code$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid tertiary effort code in row(s) #", paste0(effort_tertiary$code$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("fishery", "effortUnits"), " for a list of valid effort unit codes")))

  if(effort_tertiary$value$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing tertiary effort value in row(s) #", paste0(effort_tertiary$value$missing$row_indexes, collapse = ", "))))

  if(effort_tertiary$value$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid tertiary effort value in row(s) #", paste0(effort_tertiary$value$invalid$row_indexes, collapse = ", "))))


  same_effort_unit = checks_strata_efforts$same_unit

  same_effort_unit_ps = same_effort_unit$primary_secondary
  same_effort_unit_pt = same_effort_unit$primary_tertiary
  same_effort_unit_st = same_effort_unit$secondary_tertiary

  if(same_effort_unit_ps$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Same primary / secondary effort unit codes in row(s) #", paste0(same_effort_unit_ps$row_indexes, collapse = ", "))))

  if(same_effort_unit_pt$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Same primary / tertiary effort unit codes in row(s) #", paste0(same_effort_unit_pt$row_indexes, collapse = ", "))))

  if(same_effort_unit_st$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Same secondary / tertiary effort unit codes in row(s) #", paste0(same_effort_unit_st$row_indexes, collapse = ", "))))

  # Data issues / summary

  ## Species

  species = checks_records$species

  if(species$aggregates$number > 0) # Aggregates
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Aggregated species in column(s) #", paste0(species$aggregates$col_indexes, collapse = ", "))))

  if(species$missing$number > 0)    # Missing
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing species in column(s) #", paste0(species$missing$col_indexes, collapse = ", "))))

  if(species$invalid$number > 0)    # Invalid
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid species in column(s) #", paste0(species$invalid$col_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "species"), " for a list of valid legacy species codes")))

  ## Catch units

  catch_units = checks_records$catch_units

  if(catch_units$missing$number > 0)    # Missing
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing catch unit in column(s) #", paste0(catch_units$missing$col_indexes, collapse = ", "))))

  if(catch_units$invalid$number > 0)    # Invalid
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid catch unit in column(s) #", paste0(catch_units$invalid$col_indexes, collapse = ", "), ". Please refer to ", reference_codes("fishery", "catchUnits"), " for a list of valid catch unit codes")))

  ## Catches

  catches = checks_records$catch_values

  if(catches$positive$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$positive$number, " positive catch value(s) reported")))

  if(catches$na$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$na$number, " empty catch value(s) reported for all strata / species combinations")))

  if(catches$zero$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0(catches$zero$number, " catch value(s) explicitly reported as zero: consider leaving the cells empty instead")))

  if(catches$negative$number > 0) {
    if(catches$negative$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$negative$number, " negative catch values reported")))

    for(n in 1:nrow(catches$negative$cells)) {
      cell = catches$negative$cells[n]

      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = cell$ROW, column = cell$COL, text = paste0("Negative catch value reported in cell ", cell$INDEXES)))
    }
  }

  if(catches$non_num$number > 0) {
    if(catches$non_num$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$non_num$number, " non-numeric catch values reported")))

    for(n in 1:nrow(catches$non_num$cells)) {
      cell = catches$non_num$cells[n]

      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = cell$ROW, column = cell$COL, text = paste0("Non-numeric catch value reported in cell ", cell$INDEXES)))
    }
  }

  stratifications = checks_records$stratifications

  if(stratifications$multiple$number > 0)   # Multiple
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species-catch units in column(s) #", paste0(stratifications$multiple$col_indexes, collapse = ", "))))

  return(validation_messages)
})
