#' @include IOTC_form_CESF_class.R
#' @export IOTCForm3CE
IOTCForm3CE = setClass(
  "IOTCForm3CE",
  contains = "IOTCFormRCDI"
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

  metadata_sheet = form@metadata

  custom_metadata$data_specifications$effort_units = list(
    primary   = trim(as.character(metadata_sheet[23, 7])),
    secondary = trim(as.character(metadata_sheet[24, 7])),
    tertiary  = trim(as.character(metadata_sheet[25, 7]))
  )

  custom_metadata$data_specifications$catch_unit = trim(as.character(metadata_sheet[28, 7]))

  return(custom_metadata)
})

setMethod("validate_metadata", list(form = "IOTCForm3CE", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCForm3CE.validate_metadata")

  primary_effort_available   = is_provided(general_information$effort_units$primary)
  primary_effort_valid       = primary_effort_available & is_effort_unit_valid(general_information$effort_units$primary)

  secondary_effort_available = is_provided(general_information$effort_units$secondary)
  secondary_effort_valid     = !secondary_effort_available | is_effort_unit_valid(general_information$effort_units$secondary)

  tertiary_effort_available  = is_provided(general_information$effort_units$tertiary)
  tertiary_effort_valid      = !tertiary_effort_available | is_effort_unit_valid(general_information$effort_units$tertiary)

  custom_metadata_validation_results = common_metadata_validation_results

  custom_metadata_validation_results$general_information$effort_units =
    list(
      primary =  list(
        available = primary_effort_available,
        code      = general_information$effort_units$primary,
        valid     = primary_effort_valid
      ),
      secondary = list(
        available = secondary_effort_available,
        code      = general_information$effort_units$secondary,
        valid     = secondary_effort_valid
      ),
      tertiary = list(
        available = tertiary_effort_available,
        code      = general_information$effort_units$tertiary,
        valid     = tertiary_effort_valid
      )
    )

  catch_unit_available = is_provided(general_information$catch_unit)
  catch_unit_valid     = catch_unit_available & is_catch_unit_valid(general_information$catch_unit)

  custom_metadata_validation_results$general_information$catch_unit =
    list(
      available = catch_unit_available,
      code      = general_information$catch_unit,
      valid     = catch_unit_valid
    )

  return(custom_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCForm3CE", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm3CE.metadata_validation_summary")

  validation_messages = metadata_validation_results #new("MessageList")

  general_information    = metadata_validation_results$general_information
  effort_units           = general_information$effort_units

  # Data specifications

  ## Effort units

  ### Primary

  if(!effort_units$primary$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The primary effort type is mandatory"))
  else if(!effort_units$primary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided primary effort type (", effort_units$primary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

  ### Secondary

  if(!effort_units$secondary$available)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The secondary effort type is recommended"))
  else if(!effort_units$secondary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided secondary effort type (", effort_units$secondary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

  ### Tertiary

  if(!effort_units$tertiary$available)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The tertiary effort type is recommended"))
  else if(!effort_units$tertiary$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The provided tertiary effort type (", effort_units$tertiary$code, ") is not valid. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort type codes")))

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
  colnames(strata) = c("MONTH", "GRID_CODE", "ESTIMATION_CODE", "PRIMARY_EFFORT_CODE", "SECONDARY_EFFORT_CODE", "TERTIARY_EFFORT_CODE")

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
          "IOTCForm3CE",
          function(form, metadata_validation_results) {
            l_info("IOTCForm3CE.validate_data")

            data_validation_results = callNextMethod(form)

            strata  = form@data$strata
            records = form@data$records

            catch_data_original = records$data$catches_original

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(MONTH, GRID_CODE)]

            # If all months are provided and valid, we check that they're also consistent...
            valid_months_strata   = strata[MONTH %in% 1:12, .(NUM_MONTHS = .N), keyby = .(GRID_CODE)]

            incomplete_months_strata  = valid_quarters_strata[NUM_MONTHS < 12]

            incomplete_months  = merge(strata, incomplete_months_strata, all.x = TRUE, sort = FALSE, by = c("GRID_CODE"))
            incomplete_months  = which(!is.na(incomplete_months$NUM_MONTHS))

            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
            duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            missing_primary_effort = which(sapply(strata$RETAIN_REASON_CODE, is.na))
            invalid_retain_reasons = which(!sapply(strata$RETAIN_REASON_CODE, is_retain_reason_valid))
            invalid_retain_reasons = invalid_retain_reasons[ ! invalid_retain_reasons %in% missing_retain_reasons ]
            missing_retain_reasons = missing_retain_reasons[ ! missing_retain_reasons %in% strata_empty_rows ]

            species_occurrences = as.data.table(table(records$codes$species))
            colnames(species_occurrences) = c("SPECIES_CODE", "NUM_OCCURRENCES")

            species_occurrences_multiple = species_occurrences[NUM_OCCURRENCES > 1]

            species_multiple = which(records$codes$species %in% species_occurrences_multiple$SPECIES_CODE)

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

            data_validation_results$strata$checks$main$estimations =
              list(
                invalid = list(
                  number       = length(invalid_retain_reasons),
                  row_indexes  = invalid_retain_reasons,
                  codes        = strata[invalid_retain_reasons]$RETAIN_REASON_CODE,
                  codes_unique = unique(strata[invalid_retain_reasons]$RETAIN_REASON_CODE)
                ),
                missing = list(
                  number      = length(missing_retain_reasons),
                  row_indexes = missing_retain_reasons
                )
              )

            data_validation_results$records$checks$species$multiple =
              list(
                  number       = length(species_multiple),
                  col_indexes  = species_multiple,
                  codes_unique = species_occurrences_multiple$SPECIES_CODE
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
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "If the provided catch data refers to an industrial surface fishery then it is assumed to be raised to total annual catches and reported in live-weight equivalent by default"))

            strata  = data_validation_results$strata
            records = data_validation_results$records

            checks_strata  = strata$checks
            checks_records = records$checks

            # Strata issues / summary

            # Validation comes from the superclass

            # Strata checks

            ## Main strata

            checks_strata_main = checks_strata$main

            # Part of the validation comes from the superclass

            retain_reasons = checks_strata_main$retain_reasons

            if(retain_reasons$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing retain reason in row(s) #", paste0(retain_reasons$missing$row_indexes, collapse = ", "))))

            if(retain_reasons$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid retain reason in row(s) #", paste0(retain_reasons$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("biological", "retainReasons"), " for a list of valid retain reason codes")))

            ## Original data

            # Validation comes from the superclass

            ###

            # Data issues / summary

            ## Species

            species = checks_records$species

            # Part of the validation comes from the superclass

            if(species$multiple$number > 0)   # Multiple
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Repeated species in column(s) #", paste0(species$multiple$col_indexes, collapse = ", "))))

            ## Catches

            # Validation comes from the superclass

            return(validation_messages)
          }
)
