#' @include IOTC_form_CESF_class.R
#' @export IOTCForm3CE
IOTCForm3CE = setClass(
  "IOTCForm3CE",
  contains = "IOTCFormCESF"
)

setMethod("form_type", "IOTCForm3CE", function(form) {
  return("3-CE")
})

setMethod("form_version", "IOTCForm3CE", function(form) {
  return("1.0.0-legacy")
})

setMethod("form_dataset_code", "IOTCForm3CE", function(form) {
  return("CE")
})

setMethod("allow_empty_data", "IOTCForm3CE", function(form) {
  return(TRUE)
})

setMethod("estimation_column", "IOTCForm3CE", function(form) {
  return("D")
})

setMethod("first_data_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "H"))
})

setMethod("first_data_row", "IOTCForm3CE", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm3CE", function(form) {
  return(which(EXCEL_COLUMNS == "G"))
})

setMethod("validate_months", list(form = "IOTCForm3CE", strata = "data.table"), function(form, strata) {
  l_info("IOTCForm3CE.validate_months")

  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

  return(
    list(
      incomplete_months  = incomplete_months
    )
  )
})

setMethod("extract_metadata", list(form = "IOTCForm3CE", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCForm3CE.extract_metadata")

  custom_metadata = callNextMethod(form, common_metadata)

  metadata_sheet = form@original_metadata

  custom_metadata$data_specifications$effort_units = list(
    primary   = trim(as.character(metadata_sheet[23, 7])),
    secondary = trim(as.character(metadata_sheet[24, 7])),
    tertiary  = trim(as.character(metadata_sheet[25, 7]))
  )

  custom_metadata$data_specifications$catch_unit = trim(as.character(metadata_sheet[27, 7]))

  return(custom_metadata)
})

setMethod("validate_metadata", list(form = "IOTCForm3CE", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCForm3CE.validate_metadata")

  common_metadata_validation_results = callNextMethod(form, common_metadata_validation_results)

  data_specifications = form@metadata$data_specifications

  primary_effort_available   = is_provided(data_specifications$effort_units$primary)
  primary_effort_valid       = primary_effort_available && is_effort_unit_valid(data_specifications$effort_units$primary)

  secondary_effort_available = is_provided(data_specifications$effort_units$secondary)
  secondary_effort_valid     = !secondary_effort_available | is_effort_unit_valid(data_specifications$effort_units$secondary)

  tertiary_effort_available  = is_provided(data_specifications$effort_units$tertiary)
  tertiary_effort_valid      = !tertiary_effort_available | is_effort_unit_valid(data_specifications$effort_units$tertiary)

  custom_metadata_validation_results = common_metadata_validation_results

  custom_metadata_validation_results$data_specifications$effort_units =
    list(
      primary =  list(
        available = primary_effort_available,
        code      = data_specifications$effort_units$primary,
        valid     = primary_effort_valid
      ),
      secondary = list(
        available = secondary_effort_available,
        code      = data_specifications$effort_units$secondary,
        valid     = secondary_effort_valid
      ),
      tertiary = list(
        available = tertiary_effort_available,
        code      = data_specifications$effort_units$tertiary,
        valid     = tertiary_effort_valid
      )
    )

  catch_unit_available = is_provided(data_specifications$catch_unit)
  catch_unit_valid     = catch_unit_available && is_catch_unit_valid(data_specifications$catch_unit)

  custom_metadata_validation_results$data_specifications$catch_unit =
    list(
      available = catch_unit_available,
      code      = data_specifications$catch_unit,
      valid     = catch_unit_valid
    )

  return(custom_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCForm3CE", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm3CE.metadata_validation_summary")

  validation_messages = callNextMethod(form, metadata_validation_results) #new("MessageList")

  general_information    = metadata_validation_results$general_information
  data_specifications    = metadata_validation_results$data_specifications

  effort_units           = data_specifications$effort_units
  catch_unit             = data_specifications$catch_unit

  # Data specifications

  ## Data raising (for surface fisheries)

  fishery = general_information$fishery
  raising = data_specifications$raising

  if(fishery$valid && fishery$category == "SURFACE") {
    if(raising$valid && raising$code != "RT")
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("Data for surface fisheries (", fishery$code, ") must be fully raised to totals, i.e., data raising shall be 'RT' (currently: ", raising$code, ")")))
  }

  ## Effort units

  ### Primary

  if(!effort_units$primary$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The primary effort unit is mandatory"))
  else if(!effort_units$primary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided primary effort unit (", effort_units$primary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

  ### Secondary

  if(!effort_units$secondary$available)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The provision of a secondary effort unit is recommended"))
  else if(!effort_units$secondary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided secondary effort unit (", effort_units$secondary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

  ### Tertiary

  if(!effort_units$tertiary$available)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The tertiary effort type is recommended"))
  else if(!effort_units$tertiary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided tertiary effort unit (", effort_units$tertiary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

  ## Catch unit

  if(!catch_unit$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The catch unit type is mandatory"))
  else if(!catch_unit$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided catch unit type (", catch_unit$code, ") is not valid. Please refer to ", reference_codes("fisheries", "catchUnits"), " for a list of valid catch type codes")))

  return(validation_messages)
})

setMethod("extract_data", "IOTCForm3CE", function(form) {
  l_info("IOTCForm3CE.extract_data")

  form_metadata = form@original_metadata
  form_data     = form@original_data

  strata = form_data[4:nrow(form_data)][, 2:7]
  colnames(strata) = c("MONTH", "GRID_CODE", "ESTIMATION_CODE", "PRIMARY_EFFORT", "SECONDARY_EFFORT", "TERTIARY_EFFORT")

  strata[, MONTH    := as.integer(MONTH)]

  records = form_data[3:nrow(form_data), 8:ncol(form_data)]

  species_codes = unlist(lapply(records[1], trim), use.names = FALSE)

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  records_original = records[2:nrow(records)]
  records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata = strata,
      records =
        list(
          codes = list(
            species = species_codes
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
  l_info("IOTCForm3CE.validate_data")

  data_validation_results = callNextMethod(form, metadata_validation_results)

  strata  = form@data$strata

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata[, 1:3]) # Effort values shall not be considered, as some of them (either secondary, or tertiary, or both) might be left all empty

  strata[, IS_EMPTY := .I %in% strata_empty_rows]
  strata[, OCCURRENCES := .N, by = .(MONTH, GRID_CODE)]

  # If all months are provided and valid, we check that they're also consistent...
  valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE)]

  incomplete_months_strata  = valid_months_strata[NUM_MONTHS < 12]

  incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE"))
  incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

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

  grid_status    = data.table(FISHERY_CATEGORY_CODE = metadata_validation_results$general_information$fishery$category,
                              GRID_CODE = strata$GRID_CODE,
                              MISSING   = is.na(strata$GRID_CODE),
                              VALID     = is_grid_AR_valid(strata$GRID_CODE),
                              SIZE      = grid_size(strata$GRID_CODE))

  grid_status[, WRONG_GRID_TYPE := fifelse(FISHERY_CATEGORY_CODE == "SURFACE",
                                           SIZE != "1_DEG",
                                           fifelse(FISHERY_CATEGORY_CODE == "LONGLINE",
                                                   SIZE == "OTHER",
                                                   FALSE)
  )]

  wrong_grid_types = which(!is.na(grid_status$GRID_CODE) & grid_status$WRONG_GRID_TYPE == TRUE)

  #if(metadata_validation_results$general_information$fishery$category == "SURFACE") {
  #  wrong_grid_types = which(grid_status$SIZE != "1_DEG")
  #  wrong_grid_types = wrong_grid_types[ which(wrong_grid_types %in% which(grid_status$VALID)) ]
  #} else
  #  wrong_grid_types = as.integer(array())

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

  is_effort_valid = function(value) { return(!is.na(value) & is_numeric(value) & value > 0) }

  # Check that effort values are > 0 when provided, and that if there's an effort, then the corresponding code is
  # set in the metadata

  has_secondary_effort = FALSE
  has_tertiary_effort  = FALSE

  missing_primary_efforts = which( is.na(strata$PRIMARY_EFFORT))
  invalid_primary_efforts = which(!is_effort_valid(strata$PRIMARY_EFFORT))
  invalid_primary_efforts = invalid_primary_efforts[ ! invalid_primary_efforts %in% missing_primary_efforts ]
  missing_primary_efforts = missing_primary_efforts[ ! missing_primary_efforts %in% strata_empty_rows ]

  primary_efforts_provided= length(which(!is.na(strata$PRIMARY_EFFORT))) > 0

  missing_secondary_efforts = which( is.na(strata$SECONDARY_EFFORT))
  invalid_secondary_efforts = which(!is_effort_valid(strata$SECONDARY_EFFORT))
  invalid_secondary_efforts = invalid_secondary_efforts[ ! invalid_secondary_efforts %in% missing_secondary_efforts ]
  missing_secondary_efforts = missing_secondary_efforts[ ! missing_secondary_efforts %in% strata_empty_rows ]

  secondary_efforts_provided= length(which(!is.na(strata$SECONDARY_EFFORT))) > 0

  missing_tertiary_efforts = which( is.na(strata$TERTIARY_EFFORT))
  invalid_tertiary_efforts = which(!is_effort_valid(strata$TERTIARY_EFFORT))
  invalid_tertiary_efforts = invalid_tertiary_efforts[ ! invalid_tertiary_efforts %in% missing_tertiary_efforts ]
  missing_tertiary_efforts = missing_tertiary_efforts[ ! missing_tertiary_efforts %in% strata_empty_rows ]

  tertiary_efforts_provided= length(which(!is.na(strata$TERTIARY_EFFORT))) > 0

  data_validation_results$strata$checks$efforts = list(
    primary = list(
      unit_provided   = metadata_validation_results$data_specifications$effort_units$primary$available,
      values_provided = primary_efforts_provided,
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
    ),
    secondary = list(
      unit_provided   = metadata_validation_results$data_specifications$effort_units$secondary$available,
      values_provided = secondary_efforts_provided,
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
    ),
    tertiary = list(
      unit_provided   = metadata_validation_results$data_specifications$effort_units$tertiary$available,
      values_provided = tertiary_efforts_provided,
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

  records = form@data$records

  catch_data_original = records$data$CE_SF_data_original
  catch_data          = records$data$CE_SF_data

  species_table       = table(records$codes$species)

  if(nrow(species_table) > 0) {
    species_occurrences = as.data.table(species_table)
    colnames(species_occurrences) = c("SPECIES_CODE", "NUM_OCCURRENCES")

    species_occurrences_multiple = species_occurrences[NUM_OCCURRENCES > 1]
    species_multiple = which(records$codes$species %in% species_occurrences_multiple$SPECIES_CODE)
  } else {
    species_occurrences_multiple = data.table(SPECIES_CODE = character(), NUM_OCCURRENCIES = integer())
    species_multiple = as.integer(array())
  }

  missing_species    = which( is.na(records$codes$species))
  invalid_species    = which(!is_species_valid(records$codes$species))
  invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

  species_aggregates = which(is_species_aggregate(records$codes$species))

  numeric_catch_data =
    catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

  non_num_catches  = which(numeric_catch_data == FALSE, arr.ind = TRUE) #sum(numeric_catch_data == FALSE, na.rm = TRUE)

  na_catches       = which(numeric_catch_data == TRUE & is.na(catch_data), arr.ind = TRUE) #sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
  zero_catches     = which(numeric_catch_data == TRUE & catch_data == 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
  negative_catches = which(numeric_catch_data == TRUE & catch_data  < 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
  positive_catches = which(numeric_catch_data == TRUE & catch_data  > 0,   arr.ind = TRUE) #sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

  data_validation_results$records$checks = list(
    species = list(
      multiple = list(
        number      = length(species_multiple),
        col_indexes = spreadsheet_cols_for(form, species_multiple)
      ),
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
  data_specifications = metadata_validation_results$data_specifications

  if(fishery_info$category == "SURFACE") {
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "The provided fishery belongs to the 'surface' category and therefore catches are assumed to be reported in live-weight equivalent and raised to totals by default"))
  }

  if(data_specifications$catch_unit$code == "NO" && fishery_info$category != "LONGLINE") {
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = "Catches shall be provided in weight (either kg or t)"))
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
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(strata$checks$main$grids$invalid$number, " invalid grid code(s) reported. Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid grid codes for this dataset")))

    for(row in strata$checks$main$grids$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Invalid grid code in row #", row)))
  }

  if(strata$checks$main$grids$wrong$number > 0) {
    if(strata$checks$main$grids$wrong$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(strata$checks$main$grids$wrong$number, " grid codes refer to the wrong type of grid for the fishery")))

    for(row in strata$checks$main$grids$wrong$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Wrong type of grid for the fishery in row #", row)))
  }

  if(strata$duplicate$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

  ## Efforts

  checks_strata_efforts = checks_strata$efforts

  validation_messages = report_effort_values(validation_messages, checks_strata_efforts$primary)
  validation_messages = report_effort_values(validation_messages, checks_strata_efforts$secondary, "secondary", "F")
  validation_messages = report_effort_values(validation_messages, checks_strata_efforts$tertiary,  "tertiary",  "G")

  # Data issues / summary

  ## Species

  validation_messages = report_species(validation_messages, checks_records$species, spreadsheet_rows_for(form, 2))

  ## Catches

  validation_messages = report_catches(validation_messages, checks_records$catch_values)

  return(validation_messages)
})
