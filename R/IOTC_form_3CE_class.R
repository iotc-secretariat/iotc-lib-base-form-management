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
  effort_units           = metadata_validation_results$data_specifications$effort_units
  catch_unit             = metadata_validation_results$data_specifications$catch_unit

  # Data specifications

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
            catches_original = records_original,
            catches          = records
          )
        )
    )
  )
})

setMethod("validate_months",
          list(form = "IOTCForm3CE", strata = "data.table"),
          function(form, strata) {
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
          }
)

setMethod("validate_data",
          list(form = "IOTCForm3CE", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
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

            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
            duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            data_validation_results$strata$duplicate =
              list(
                number = length(duplicate_strata),
                row_indexes = duplicate_strata
              )

            data_validation_results$strata$unique =
              list(
                number = length(unique_strata),
                row_indexes = unique_strata
              )

            is_effort_valid = function(value) { return(!is.na(value) | value > 0) }

            # Check that effort values are > 0 when provided, and that if there's an effort, then the corresponding code is
            # set in the metadata

            has_secondary_effort = FALSE
            has_tertiary_effort  = FALSE

            missing_primary_efforts = which(sapply(strata$PRIMARY_EFFORT, is.na))
            invalid_primary_efforts = which(!sapply(strata$PRIMARY_EFFORT, is_effort_valid))
            invalid_primary_efforts = invalid_primary_efforts[ ! invalid_primary_efforts %in% missing_primary_efforts ]
            missing_primary_efforts = missing_primary_efforts[ ! missing_primary_efforts %in% strata_empty_rows ]

            primary_efforts_provided= length(which(!sapply(strata$PRIMARY_EFFORT, is.na))) > 0

            missing_secondary_efforts = which(sapply(strata$SECONDARY_EFFORT, is.na))
            invalid_secondary_efforts = which(!sapply(strata$SECONDARY_EFFORT, is_effort_valid))
            invalid_secondary_efforts = invalid_secondary_efforts[ ! invalid_secondary_efforts %in% missing_secondary_efforts ]
            missing_secondary_efforts = missing_secondary_efforts[ ! missing_secondary_efforts %in% strata_empty_rows ]

            secondary_efforts_provided= length(which(!sapply(strata$SECONDARY_EFFORT, is.na))) > 0

            missing_tertiary_efforts = which(sapply(strata$TERTIARY_EFFORT, is.na))
            invalid_tertiary_efforts = which(!sapply(strata$TERTIARY_EFFORT, is_effort_valid))
            invalid_tertiary_efforts = invalid_tertiary_efforts[ ! invalid_tertiary_efforts %in% missing_tertiary_efforts ]
            missing_tertiary_efforts = missing_tertiary_efforts[ ! missing_tertiary_efforts %in% strata_empty_rows ]

            tertiary_efforts_provided= length(which(!sapply(strata$TERTIARY_EFFORT, is.na))) > 0

            data_validation_results$strata$checks$efforts = list(
              primary = list(
                  unit_provided   = metadata_validation_results$data_specifications$effort_units$primary$available,
                  values_provided = primary_efforts_provided,
                  missing = list(
                    number      = length(missing_primary_efforts),
                    row_indexes = missing_primary_efforts
                  ),
                  invalid = list(
                    number        = length(invalid_primary_efforts),
                    row_indexes   = invalid_primary_efforts,
                    values        = strata$PRIMARY_EFFORT[invalid_primary_efforts],
                    values_unique = unique(strata$PRIMARY_EFFORT[invalid_primary_efforts])
                  )
              ),
              secondary = list(
                unit_provided   = metadata_validation_results$data_specifications$effort_units$secondary$available,
                values_provided = secondary_efforts_provided,
                missing = list(
                  number      = length(missing_secondary_efforts),
                  row_indexes = missing_secondary_efforts
                ),
                invalid = list(
                  number        = length(invalid_secondary_efforts),
                  row_indexes   = invalid_secondary_efforts,
                  values        = strata$SECONDARY_EFFORT[invalid_secondary_efforts],
                  values_unique = unique(strata$SECONDARY_EFFORT[invalid_secondary_efforts])
                )
              ),
              tertiary = list(
                unit_provided   = metadata_validation_results$data_specifications$effort_units$tertiary$available,
                values_provided = tertiary_efforts_provided,
                missing = list(
                  number      = length(missing_tertiary_efforts),
                  row_indexes = missing_tertiary_efforts
                ),
                invalid = list(
                  number        = length(invalid_tertiary_efforts),
                  row_indexes   = invalid_tertiary_efforts,
                  values        = strata$TERTIARY_EFFORT[invalid_tertiary_efforts],
                  values_unique = unique(strata$TERTIARY_EFFORT[invalid_tertiary_efforts])
                )
              )
            )

            records = form@data$records

            catch_data_original = records$data$catches_original
            catch_data          = records$data$catches

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

            missing_species    = which( sapply(records$codes$species, is.na))
            invalid_species    = which(!sapply(records$codes$species, is_species_valid))
            invalid_species    = invalid_species[ ! invalid_species %in% missing_species ]

            species_aggregates = which(unlist(sapply(records$codes$species, function(value) { return(ifelse(is.na(value), FALSE, is_species_aggregate(value))) }, USE.NAMES = FALSE)))

            numeric_catch_data =
              catch_data_original[, lapply(.SD, function(value) { lapply(value, function(v) { is.na(v) | is_numeric(v) }) })]

            non_num_catches  = sum(numeric_catch_data == FALSE, na.rm = TRUE)

            na_catches       = sum(numeric_catch_data == TRUE & is.na(catch_data), na.rm = TRUE)
            zero_catches     = sum(numeric_catch_data == TRUE & catch_data == 0,   na.rm = TRUE)
            negative_catches = sum(numeric_catch_data == TRUE & catch_data  < 0,   na.rm = TRUE)
            positive_catches = sum(numeric_catch_data == TRUE & catch_data  > 0,   na.rm = TRUE)

            data_validation_results$records$checks = list(
              species = list(
                missing = list(
                  number      = length(missing_species),
                  col_indexes = missing_species
                ),
                invalid = list(
                  number       = length(invalid_species),
                  col_indexes  = invalid_species,
                  codes        = records$codes$species[invalid_species],
                  codes_unique = unique(records$codes$species[invalid_species])
                ),
                aggregates = list(
                  number      = length(species_aggregates),
                  col_indexes = species_aggregates,
                  codes       = records$codes$species[species_aggregates]
                )
              ),
              catch_values = list(
                na       = na_catches,
                zero     = zero_catches,
                positive = positive_catches,
                negative = negative_catches,
                non_num  = non_num_catches
              )
            )

            return(data_validation_results)
          }
)

setMethod("data_validation_summary",
          "IOTCForm3CE",
          #list(form = "IOTCForm3CEDI", data_validation_results = "list"),
          function(form, data_validation_results) {
            l_info("IOTCForm3CE.data_validation_summary")

            validation_messages = common_data_validation_summary(form, data_validation_results)

            ### STRATA AND RECORDS

            # This is only true for 3CE / surface
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "If the provided catch data refer to an industrial surface fishery then it is assumed to be raised to total annual catches and reported in live-weight equivalent by default"))

            strata  = data_validation_results$strata
            records = data_validation_results$records

            checks_strata  = strata$checks
            checks_records = records$checks

            # Strata issues / summary

            # Validation comes from the superclass

            # Strata checks

            ## Main strata

            if(strata$duplicate$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

            ## Efforts

            checks_strata_efforts = checks_strata$efforts

            effort_primary   = checks_strata_efforts$primary

            if(effort_primary$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing primary effort value in row(s) #", paste0(effort_primary$missing$row_indexes, collapse = ", "))))

            if(effort_primary$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid primary effort value in row(s) #", paste0(effort_primary$invalid$row_indexes, collapse = ", "))))

            effort_secondary = checks_strata_efforts$secondary

            if(effort_secondary$unit_provided) {
              if(effort_secondary$missing$number > 0)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing secondary effort value in row(s) #", paste0(effort_secondary$missing$row_indexes, collapse = ", "))))

              if(effort_secondary$invalid$number > 0)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid secondary effort value in row(s) #", paste0(effort_secondary$invalid$row_indexes, collapse = ", "))))
            } else {
              if(effort_secondary$values_provided) {
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = "Secondary effort values provided, but no secondary effort unit is found in the metadata"))
              }
            }

            effort_tertiary  = checks_strata_efforts$tertiary

            if(effort_tertiary$unit_provided) {
              if(effort_tertiary$missing$number > 0)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing tertiary effort value in row(s) #", paste0(effort_tertiary$missing$row_indexes, collapse = ", "))))

              if(effort_tertiary$invalid$number > 0)
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid tertiary effort value in row(s) #", paste0(effort_tertiary$invalid$row_indexes, collapse = ", "))))
            } else {
              if(effort_tertiary$values_provided) {
                validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = "Tertiary effort values provided, but no tertiary effort unit is found in the metadata"))
              }
            }

            # Data issues / summary

            ## Species

            species = checks_records$species

            if(species$aggregates$number > 0) # Aggregates
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Aggregated species in column(s) #", paste0(species$aggregates$col_indexes, collapse = ", "))))

            if(species$missing$number > 0)    # Missing
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing species in column(s) #", paste0(species$missing$col_indexes, collapse = ", "))))

            if(species$invalid$number > 0)    # Invalid
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid species in column(s) #", paste0(species$invalid$col_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "speciess"), " for a list of valid legacy species codes")))

            ## Catches

            catches = checks_records$catch_values

            if(catches$positive > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$positive, " positive catch value(s) reported")))

            if(catches$na > 0)
              validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(catches$na, " empty catch value(s) reported for all strata / species combinations")))

            if(catches$zero > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0(catches$zero, " catch value(s) explicitly reported as zero: consider leaving the cells empty instead")))

            if(catches$negative > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$negative, " negative catch value(s) reported")))

            if(catches$non_num > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(catches$non_num, " non-numeric catch value(s) reported")))

            return(validation_messages)
          }
)
