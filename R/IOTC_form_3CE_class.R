#' @include IOTC_form_CESF_class.R
#' @export IOTCForm3CE
IOTCForm3CE = setClass(
  "IOTCForm3CE",
  contains = "IOTCFormCESF"
)

setMethod("form_type", "IOTCForm3CE", function(form) {
  return(c("3-CE", "3CE")) # For backwards compatibility
})

setMethod("form_version", "IOTCForm3CE", function(form) {
  return("1.0.0")
})

setMethod("form_dataset_code", "IOTCForm3CE", function(form) {
  return("CE")
})

setMethod("grid_validator", "IOTCForm3CE", function(form) {
  return(is_grid_AR_valid)
})

setMethod("allow_empty_data", "IOTCForm3CE", function(form) {
  return(TRUE)
})

setMethod("estimation_column", "IOTCForm3CE", function(form) {
  return("E")
})

setMethod("optional_strata_columns", "IOTCForm3CE", function(form) {
  return(14:17) # Secondary and tertiary effort codes / values
})

setMethod("first_data_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "S"))
})

setMethod("first_data_row", "IOTCForm3CE", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "Q"))
})

setMethod("validate_months", "IOTCForm3CE", function(form) {
  start = Sys.time()

  strata = form@data$processed_strata

  l_info("IOTCForm3CE.validate_months")

  # Originally:
  #valid_months_strata   = strata[!is.na(MONTH_ORIGINAL) & MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, GRID_CODE, DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  # Now: (inspired by IOTC_form_4SF_class.R)
  # It's more sensible to provide a warning when not all months are provided by ** FISHERY and GRID ** only
  # rather than by the full stratum definition
  valid_months_strata   = strata[!is.na(MONTH_ORIGINAL) & MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, GRID_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  # Originally:
  #incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "GRID_CODE", "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))

  # Now: (inspired by IOTC_form_4SF_class.R)
  # See comment above
  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "GRID_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_info(paste0("IOTCForm3CE.validate_months: ", Sys.time() - start))

  return(
    list(
      incomplete_months  = incomplete_months
    )
  )
})

setMethod("extract_data", "IOTCForm3CE", function(form) {
  l_info("IOTCForm3CE.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  has_data = nrow(form_data) >= 4

  strata = form_data[4:nrow(form_data)][, first_strata_column(form):last_strata_column(form)]

  if(!has_data) {
    strata = as.data.table(matrix(nrow = 0, ncol = length(colnames(strata))))
  }

  colnames(strata) = c("MONTH", "FISHERY_CODE", "GRID_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "PRIMARY_EFFORT_CODE", "PRIMARY_EFFORT", "SECONDARY_EFFORT_CODE", "SECONDARY_EFFORT", "TERTIARY_EFFORT_CODE", "TERTIARY_EFFORT")

  strata[, MONTH_ORIGINAL := MONTH]
  strata[, MONTH          := as.integer(MONTH)]

  records = form_data[2:nrow(form_data), first_data_column(form):ncol(form_data)]

  species_codes    = unlist(lapply(records[1], trim), use.names = FALSE)
  if(length(species_codes) == 1 && is.na(species_codes[1])) species_codes = NA

  catch_unit_codes = unlist(lapply(records[2], trim), use.names = FALSE)
  if(length(catch_unit_codes) == 1 && is.na(catch_unit_codes[1])) catch_unit_codes = NA

  if(has_data) {
    # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
    records_original = records[3:nrow(records)]
    records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]
  } else {
    records_original = as.data.table(matrix(nrow = 0, ncol = ifelse(is_available(species_codes), 0, length(species_codes))))

    #Was !is_available...
    if(is_available(species_codes)) colnames(records_original) = species_codes
    records          = records_original
  }

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

setMethod("validate_data", list(form = "IOTCForm3CE", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start = Sys.time()

  l_info("IOTCForm3CE.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  l_info(paste0("IOTCForm3CE.validate_data.super(): ", Sys.time() - start))
  start = Sys.time()

  strata  = form@data$strata
  strata$IS_EMPTY = NULL # Otherwise the 'find_empty_rows' call below will never return anything meaningful...

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata[, 1:3]) # Effort values shall not be considered, as some of them (either secondary, or tertiary, or both) might be left all empty

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(MONTH, FISHERY_CODE, GRID_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  # If all months are provided and valid, we check that they're also consistent...
  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(FISHERY_CODE, GRID_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "GRID_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  l_info(paste0("IOTCForm3CE.validate_data (I): ", Sys.time() - start))
  start = Sys.time()

  grid_size = function(code) {
    return(
      fifelse(is.na(code) | code == "",
              "OTHER",
              fifelse(str_sub(code, 1, 1) == "5",
                      "1_DEG",
                      fifelse(str_sub(code, 1, 1) == "6",
                              "5_DEG",
                              "OTHER"
                      )
              )
      )
    )
  }

  # Merges the fishery codes in the strata with the FISHERIES table in order to recover the fishery category
  fishery_categories = merge(strata, iotc.data.reference.codelists::FISHERIES[, .(CODE, FISHERY_CATEGORY_CODE)],
                             by.x = "FISHERY_CODE", by.y = "CODE",
                             sort = FALSE)

  grid_status    = data.table(FISHERY_CODE = fishery_categories$FISHERY_CODE,
                              FISHERY_CATEGORY_CODE = fishery_categories$FISHERY_CATEGORY_CODE,
                              GRID_CODE = strata$GRID_CODE,
                              MISSING   = is.na(strata$GRID_CODE),
                              VALID     = grid_validator(form)(strata$GRID_CODE),
                              SIZE      = grid_size(strata$GRID_CODE))

  l_info(paste0("IOTCForm3CE.validate_data (I.a): ", Sys.time() - start))
  start = Sys.time()

  grid_status[, WRONG_GRID_TYPE := fifelse(FISHERY_CATEGORY_CODE == "SURFACE",
                                           SIZE != "1_DEG",
                                           fifelse(FISHERY_CATEGORY_CODE == "LONGLINE",
                                                   SIZE == "OTHER",
                                                   FALSE)
                                           )]

  l_info(paste0("IOTCForm3CE.validate_data (I.b): ", Sys.time() - start))
  start = Sys.time()

  wrong_grid_types = which(!is.na(grid_status$GRID_CODE) & grid_status$WRONG_GRID_TYPE == TRUE)

  l_info(paste0("IOTCForm3CE.validate_data (II.a): ", Sys.time() - start))
  start = Sys.time()

  data_validation_results$strata$checks$main$grids$wrong = list(
    number       = length(wrong_grid_types),
    row_indexes  = spreadsheet_rows_for(form, wrong_grid_types),
    codes        = strata$GRID_CODE[wrong_grid_types],
    codes_unique = unique(strata$GRID_CODE[wrong_grid_types])
  )

  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
  duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
  duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
  unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

  l_info(paste0("IOTCForm3CE.validate_data (II.b): ", Sys.time() - start))
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

  l_info(paste0("IOTCForm3CE.validate_data (II.c): ", Sys.time() - start))
  start = Sys.time()

  is_effort_valid = function(value) { return(!is.na(value) & is_numeric(value) & value > 0) }

  ### Primary effort code + value

  missing_primary_effort_codes = which( is.na(strata$PRIMARY_EFFORT_CODE))

  l_info(paste0("IOTCForm3CE.validate_data (II.d): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_effort_codes = which(!is_effort_unit_valid(strata$PRIMARY_EFFORT_CODE))

  l_info(paste0("IOTCForm3CE.validate_data (II.e): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_effort_codes = invalid_primary_effort_codes[ ! invalid_primary_effort_codes %in% missing_primary_effort_codes ]
  missing_primary_effort_codes = missing_primary_effort_codes[ ! missing_primary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CE.validate_data (III): ", Sys.time() - start))
  start = Sys.time()

  missing_primary_efforts = which( is.na(strata$PRIMARY_EFFORT))

  l_info(paste0("IOTCForm3CE.validate_data (III.a): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_efforts = which(!is_effort_valid(strata$PRIMARY_EFFORT))

  l_info(paste0("IOTCForm3CE.validate_data (III.b): ", Sys.time() - start))
  start = Sys.time()

  invalid_primary_efforts = invalid_primary_efforts[ ! invalid_primary_efforts %in% missing_primary_efforts ]
  missing_primary_efforts = missing_primary_efforts[ ! missing_primary_efforts %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CE.validate_data (IV): ", Sys.time() - start))
  start = Sys.time()

  ### Secondary effort code + value

  missing_secondary_effort_codes = which( is.na(strata$SECONDARY_EFFORT_CODE))
  invalid_secondary_effort_codes = which(!is_effort_unit_valid(strata$SECONDARY_EFFORT_CODE))
  invalid_secondary_effort_codes = invalid_secondary_effort_codes[ ! invalid_secondary_effort_codes %in% missing_secondary_effort_codes ]
  missing_secondary_effort_codes = missing_secondary_effort_codes[ ! missing_secondary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CE.validate_data (V): ", Sys.time() - start))
  start = Sys.time()

  missing_secondary_efforts = which( is.na(strata$SECONDARY_EFFORT))
  invalid_secondary_efforts = which(!is_effort_valid(strata$SECONDARY_EFFORT))
  invalid_secondary_efforts = invalid_secondary_efforts[ ! invalid_secondary_efforts %in% missing_secondary_efforts ]
  missing_secondary_efforts = missing_secondary_efforts[ ! missing_secondary_efforts %in% strata_empty_rows ]

  missing_secondary_efforts_      = missing_secondary_efforts     [which(!missing_secondary_efforts      %in% missing_secondary_effort_codes)]
  missing_secondary_effort_codes_ = missing_secondary_effort_codes[which(!missing_secondary_effort_codes %in% missing_secondary_efforts)]

  missing_secondary_efforts      = missing_secondary_efforts_
  missing_secondary_effort_codes = missing_secondary_effort_codes_

  l_info(paste0("IOTCForm3CE.validate_data (VI): ", Sys.time() - start))
  start = Sys.time()

  ### Tertiary effort code + value

  missing_tertiary_effort_codes = which( is.na(strata$TERTIARY_EFFORT_CODE))
  invalid_tertiary_effort_codes = which(!is_effort_unit_valid(strata$TERTIARY_EFFORT_CODE))
  invalid_tertiary_effort_codes = invalid_tertiary_effort_codes[ ! invalid_tertiary_effort_codes %in% missing_tertiary_effort_codes ]
  missing_tertiary_effort_codes = missing_tertiary_effort_codes[ ! missing_tertiary_effort_codes %in% strata_empty_rows ]

  l_info(paste0("IOTCForm3CE.validate_data (VII): ", Sys.time() - start))
  start = Sys.time()

  missing_tertiary_efforts = which( is.na(strata$TERTIARY_EFFORT))
  invalid_tertiary_efforts = which(!is_effort_valid(strata$TERTIARY_EFFORT))
  invalid_tertiary_efforts = invalid_tertiary_efforts[ ! invalid_tertiary_efforts %in% missing_tertiary_efforts ]
  missing_tertiary_efforts = missing_tertiary_efforts[ ! missing_tertiary_efforts %in% strata_empty_rows ]

  missing_tertiary_efforts_      = missing_tertiary_efforts     [which(!missing_tertiary_efforts      %in% missing_tertiary_effort_codes)]
  missing_tertiary_effort_codes_ = missing_tertiary_effort_codes[which(!missing_tertiary_effort_codes %in% missing_tertiary_efforts)]

  missing_tertiary_efforts      = missing_tertiary_efforts_
  missing_tertiary_effort_codes = missing_tertiary_effort_codes_

  same_effort_unit_ps = which(strata[!is.na(PRIMARY_EFFORT_CODE),   SAME_EFFORT_UNITS_PS := PRIMARY_EFFORT_CODE   == SECONDARY_EFFORT_CODE]$SAME_EFFORT_UNITS_PS == TRUE)
  same_effort_unit_pt = which(strata[!is.na(PRIMARY_EFFORT_CODE),   SAME_EFFORT_UNITS_PT := PRIMARY_EFFORT_CODE   == TERTIARY_EFFORT_CODE ]$SAME_EFFORT_UNITS_PT == TRUE)
  same_effort_unit_st = which(strata[!is.na(SECONDARY_EFFORT_CODE), SAME_EFFORT_UNITS_ST := SECONDARY_EFFORT_CODE == TERTIARY_EFFORT_CODE ]$SAME_EFFORT_UNITS_ST == TRUE)

  l_info(paste0("IOTCForm3CE.validate_data (VIII): ", Sys.time() - start))
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

  l_info(paste0("IOTCForm3CE.validate_data (IX): ", Sys.time() - start))
  start = Sys.time()

  ### Maybe a check shall be added that for surface / coastal fisheries catches shall be necessarily provided in weight?

  missing_species    = which( is.na(records$codes$species))
  invalid_species    = which(!is_species_valid(records$codes$species))
  invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

  l_info(paste0("IOTCForm3CE.validate_data (X): ", Sys.time() - start))
  start = Sys.time()

  species_aggregates = which(is_species_aggregate(records$codes$species))

  l_info(paste0("IOTCForm3CE.validate_data (XI): ", Sys.time() - start))
  start = Sys.time()

  missing_catch_units = which( is.na(records$codes$catch_units))
  invalid_catch_units = which(!is_catch_unit_valid(records$codes$catch_units))
  invalid_catch_units = invalid_catch_units[ ! invalid_catch_units %in% missing_catch_units ]

  l_info(paste0("IOTCForm3CE.validate_data (XII): ", Sys.time() - start))
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

  l_info(paste0("IOTCForm3CE.validate_data (XIII): ", Sys.time() - start))
  start = Sys.time()

  numeric_catch_data =
    catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

  l_info(paste0("IOTCForm3CE.validate_data (XIV): ", Sys.time() - start))
  start = Sys.time()

  non_num_catches  = which(numeric_catch_data == FALSE, arr.ind = TRUE) #sum(numeric_catch_data == FALSE, na.rm = TRUE)

  na_catches       = which(numeric_catch_data == TRUE & is.na(catch_data), arr.ind = TRUE) #sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
  zero_catches     = which(numeric_catch_data == TRUE & catch_data == 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
  negative_catches = which(numeric_catch_data == TRUE & catch_data  < 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
  positive_catches = which(numeric_catch_data == TRUE & catch_data  > 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

  l_info(paste0("IOTCForm3CE.validate_data (XV): ", Sys.time() - start))
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

  l_info(paste0("IOTCForm3CE.validate_data: ", Sys.time() - start))

  return(data_validation_results)
})

setMethod("data_validation_summary", list(form = "IOTCForm3CE", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  l_info("IOTCForm3CE.data_validation_summary")

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

    if(!is.na(data_specifications$catch_unit$code) && data_specifications$catch_unit$code == "NO" &&
       !is.na(fishery_info$category) &&  fishery_info$category != "LONGLINE") {
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

    for(row in strata$checks$main$grids$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "D", text = paste0("Invalid grid code in row #", row)))
  }

  if(strata$checks$main$grids$wrong$number > 0) {
    if(strata$checks$main$grids$wrong$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(strata$checks$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery")))

    for(row_index in strata$checks$main$grids$wrong$row_indexes) {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row_index, column = "D", text = paste0("Wrong grid code for fishery in row #", row_index)))
    }
  }

  # Commented out as the message is handled at superclass level
  #if(strata$duplicate$number > 0)
  #  validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

  ## Efforts

  checks_strata_efforts = checks_strata$efforts

  effort_primary   = checks_strata_efforts$primary

  validation_messages = report_effort_multiple(validation_messages, checks_strata_efforts$primary)
  validation_messages = report_effort_multiple(validation_messages, checks_strata_efforts$secondary, "secondary", "N", "O")
  validation_messages = report_effort_multiple(validation_messages, checks_strata_efforts$tertiary,  "tertiary",  "P", "Q")

  same_effort_unit = checks_strata_efforts$same_unit

  same_effort_unit_ps = same_effort_unit$primary_secondary
  same_effort_unit_pt = same_effort_unit$primary_tertiary
  same_effort_unit_st = same_effort_unit$secondary_tertiary

  if(same_effort_unit_ps$number > 0) {
    if(same_effort_unit_ps$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "L & N", text = paste0(same_effort_unit_ps$number, " records with same primary / secondary effort unit codes")))

    for(row in same_effort_unit_ps$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "L & N", text = paste0("Same primary / secondary effort unit codes in row #", row)))
  }

  if(same_effort_unit_pt$number > 0) {
    if(same_effort_unit_pt$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "L & P", text = paste0(same_effort_unit_pt$number, " records with same primary / tertiary effort unit codes")))

    for(row in same_effort_unit_pt$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "L & P", text = paste0("Same primary / tertiary effort unit codes in row #", row)))
  }

  if(same_effort_unit_st$number > 0) {
    if(same_effort_unit_st$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "N & P", text = paste0(same_effort_unit_st$number, " records with same secondary / tertiary effort unit codes")))

    for(row in same_effort_unit_st$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "N & P", text = paste0("Same secondary / tertiary effort unit codes in row #", row)))
  }

  # Data issues / summary

  ## Species

  validation_messages = report_species(validation_messages,checks_records$species, spreadsheet_rows_for(form, 2))

  ## Catch units

  catch_units = checks_records$catch_units
  catch_units_row = spreadsheet_rows_for(form, 2)

  if(catch_units$missing$number > 0) {    # Missing
    if(catch_units$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = catch_units_row, text = paste0(catch_units$missing$number, " missing catch unit codes")))

    for(col in catch_units$missing$col_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = catch_units_row, column = col, text = paste0("Missing catch unit code in column ", col)))
  }

  if(catch_units$invalid$number > 0) {    # Invalid
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = catch_units_row, text = paste0(catch_units$invalid$number, " invalid catch unit codes. Please refer to ", reference_codes("fishery", "catchUnits"), " for a list of valid catch unit codes")))

    for(col in catch_units$invalid$col_indexes) {
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = catch_units_row, column = col, text = paste0("Invalid catch unit in column ", col)))
    }
  }

  ## Catches

  validation_messages = report_catches(validation_messages,checks_records$catch_values)

  ## Strata

  stratifications = checks_records$stratifications

  if(stratifications$multiple$number > 0)   # Multiple
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species-catch units in column(s) #", paste0(stratifications$multiple$col_indexes, collapse = ", "))))

  return(validation_messages)
})

## OUTPUT

setMethod("extract_output", list(form = "IOTCForm3CE", wide = "logical"),
          function(form, wide) {
            form = read(form)

            form_metadata = extract_metadata(form, common_metadata(form@original_metadata))
            form_data     = extract_data(form)

            strata = form_data$strata
            data   = form_data$records$data$CE_SF_data

            species_unit = paste0(form_data$records$codes$species, "_",
                                  form_data$records$codes$catch_units)

            colnames(data) = species_unit

            year = form_metadata$general_information$reporting_year
            fleet = fleets_for(form_metadata$general_information$reporting_entity,
                               form_metadata$general_information$flag_country)

            strata$YEAR                  = year
            strata$REPORTING_ENTITY_CODE = form_metadata$general_information$reporting_entity
            strata$FLAG_COUNTRY_CODE     = form_metadata$general_information$flag_country
            strata$FLEET_CODE            = fleet$FLEET_CODE

            # Not required when using the new fishery codes
            #strata = merge(strata, FISHERY_MAPPINGS, by = "FISHERY_CODE", all.x = TRUE, sort = FALSE)

            strata = strata[, .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                YEAR, MONTH,
                                FISHERY_CODE,
                               #GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                GRID_CODE, ESTIMATION_CODE,
                                PRIMARY_EFFORT_CODE, PRIMARY_EFFORT, SECONDARY_EFFORT_CODE, SECONDARY_EFFORT, TERTIARY_EFFORT_CODE, TERTIARY_EFFORT)]

            data = data[, lapply(.SD, function(value) { return(ifelse(is.na(value) | value == 0, NA_real_, round(as.numeric(value), 2))) })]

            output_data = cbind(strata, data)

            if(!wide) {
              output_data = melt.data.table(output_data,
                                            id.vars = 1:24,
                                            value.name = "CATCH",
                                            variable.name = "SPECIES_UNIT_CODE")

              species_unit = str_split(string = output_data$SPECIES_UNIT_CODE,
                                       pattern = "\\_",
                                       simplify = TRUE)

              output_data[, SPECIES_CODE    := species_unit[, 1]]
              output_data[, CATCH_UNIT_CODE := species_unit[, 2]]

              output_data$SPECIES_UNIT_CODE = NULL

              output_data =
                output_data[, .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                YEAR, MONTH,
                                FISHERY_CODE,
                               #GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                GRID_CODE, ESTIMATION_CODE,
                                PRIMARY_EFFORT_CODE, PRIMARY_EFFORT, SECONDARY_EFFORT_CODE, SECONDARY_EFFORT, TERTIARY_EFFORT_CODE, TERTIARY_EFFORT,
                                SPECIES_CODE, CATCH, CATCH_UNIT_CODE)]

              # To remove meaningless records (i.e., those with species and / or catch unit code set, but with NA as catch) and enable correct handlign of records with efforts only (for a given strata)
              output_data[is.na(CATCH) | CATCH == 0, `:=`(CATCH = NA, SPECIES_CODE = NA, CATCH_UNIT_CODE = NA)]

              output_data[, TOTAL_CATCH := sum(CATCH, na.rm = TRUE), by = .(REPORTING_ENTITY_CODE, FLAG_COUNTRY_CODE, FLEET_CODE,
                                                                            YEAR, MONTH,
                                                                            FISHERY_CODE,
                                                                           #GEAR_CODE, MAIN_GEAR_CODE, SCHOOL_TYPE_CODE,
                                                                            DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE, COVERAGE_TYPE_CODE, COVERAGE,
                                                                            GRID_CODE, ESTIMATION_CODE,
                                                                            PRIMARY_EFFORT_CODE, PRIMARY_EFFORT, SECONDARY_EFFORT_CODE, SECONDARY_EFFORT, TERTIARY_EFFORT_CODE, TERTIARY_EFFORT)]

              output_data = unique(output_data)[is.na(TOTAL_CATCH) | CATCH > 0]

              output_data$TOTAL_CATCH = NULL # Not required anymore
            }

            return(output_data)
          }
)
