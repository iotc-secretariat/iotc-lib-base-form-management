#' @include IOTC_form_RCDI_class.R
#' @export IOTCForm1RC
IOTCForm1RC = setClass(
  "IOTCForm1RC",
  contains = "IOTCFormRCDI"
)

setMethod("form_type", "IOTCForm1RC", function(form) {
  return("1-RC")
})

setMethod("form_version", "IOTCForm1RC", function(form) {
  return("1.0.0-legacy")
})

setMethod("form_dataset_code", "IOTCForm1RC", function(form) {
  return("RC")
})

setMethod("extract_data", "IOTCForm1RC", function(form) {
  form_metadata = form@original_metadata
  form_data     = form@original_data

  strata = form_data[4:nrow(form_data)][, 2:11]
  colnames(strata) = c("QUARTER", "FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, QUARTER    := as.integer(QUARTER)]
  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[3:nrow(form_data), 12:ncol(form_data)]

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

setMethod("validate_quarters",
          list(form = "IOTCForm1RC", strata = "data.table"),
          function(form, strata) {
            l_info("IOTCForm1RC.validate_quarters")

            all_year_quarter_strata = unique(strata[QUARTER == 0, .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
            valid_quarters_strata   = strata[QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

            overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

            incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))

            return(
              list(
                overlapping_quarters = overlapping_quarters,
                incomplete_quarters  = incomplete_quarters
              )
            )
          }
)

setMethod("validate_data",
          "IOTCForm1RC",
          function(form) {
            l_info("IOTCForm1RC.validate_data")

            data_validation_results = callNextMethod(form)

            strata  = form@data$strata
            records = form@data$records

            catch_data_original = records$data$catches_original

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(QUARTER, FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            # If all quarters are provided and valid, we check that they're also consistent...
            all_year_quarter_strata = unique(strata[QUARTER == 0, .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)])
            valid_quarters_strata   = strata[QUARTER %in% 1:4, .(NUM_QUARTERS = .N), keyby = .(FISHERY_CODE, TARGET_SPECIES_CODE, IOTC_MAIN_AREA_CODE, RETAIN_REASON_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE)]

            overlapping_quarters_strata = merge(all_year_quarter_strata, valid_quarters_strata, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters_strata  = valid_quarters_strata[NUM_QUARTERS < 4]

            overlapping_quarters = merge(strata, overlapping_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            overlapping_quarters = which(!is.na(overlapping_quarters$NUM_QUARTERS))

            incomplete_quarters  = merge(strata, incomplete_quarters_strata, all.x = TRUE, sort = FALSE, by = c("FISHERY_CODE", "TARGET_SPECIES_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE"))
            incomplete_quarters  = which(!is.na(incomplete_quarters$NUM_QUARTERS))

            non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]
            duplicate_strata = which(strata$OCCURRENCES > 1)   #which(strata_duplicated$COUNT > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            missing_retain_reasons = which(sapply(strata$RETAIN_REASON_CODE, is.na))
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

            data_validation_results$strata$checks$main$retain_reasons =
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
          "IOTCForm1RC",
          function(form, data_validation_results) {
            l_info("IOTCForm1RC.data_validation_summary")

            validation_messages = common_data_validation_summary(form, data_validation_results)

            ### STRATA AND RECORDS

            # This is only true for 1RC, whereas for 1DI the total should be raised, but can be expressed also as total numbers
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = "Provided catch data is assumed to be raised to total annual catches and reported in live-weight equivalent by default"))

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
