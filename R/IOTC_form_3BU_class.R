#' @include IOTC_form_RCDI_class.R
#' @export IOTCForm3BU
IOTCForm3BU = setClass(
  "IOTCForm3BU",
  contains = "IOTCForm"
)

setMethod("form_type", "IOTCForm3BU", function(form) {
  return("3-BU")
})

setMethod("form_version", "IOTCForm3BU", function(form) {
  return("1.0.0")
})

setMethod("form_dataset_code", "IOTCForm3BU", function(form) {
  return("BU")
})

setMethod("first_data_column", "IOTCForm3BU", function(form) {
  return(which(EXCEL_COLUMNS == "D"))
})

setMethod("first_data_row", "IOTCForm3BU", function(form) {
  return(6)
})

setMethod("first_strata_column", "IOTCForm3BU", function(form) {
  return(which(EXCEL_COLUMNS == "B"))
})

setMethod("last_strata_column", "IOTCForm3BU", function(form) {
  return(which(EXCEL_COLUMNS == "C"))
})

setMethod("form_comment_cell_row", "IOTCForm3BU", function(form) {
  return(23)
})

setMethod("extract_metadata", list(form = "IOTCForm3BU", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCForm3BU.extract_metadata")

  metadata_sheet = form@original_metadata

  metadata = common_metadata

  metadata$general_information$reporting_month = as.integer(trim(as.character(metadata_sheet[16, 7])))
  metadata$general_information$vessel = list(
    name = trim(as.character(metadata_sheet[17, 7])),
    IOTC_number = trim(as.character(metadata_sheet[18, 7]))
  )

  return(metadata)
})

setMethod("extract_data", "IOTCForm3BU", function(form) {
  form_metadata = form@original_metadata
  form_data     = form@original_data

  strata = form_data[(first_data_row(form) - 2):nrow(form_data)][, first_strata_column(form):last_strata_column(form)]
  colnames(strata) = c("DAY_OF_MONTH", "BUOY_ID")

  strata[, DAY_OF_MONTH := as.integer(DAY_OF_MONTH)]
  strata[, BUOY_ID      := trim(as.character(BUOY_ID))]

  records = form_data[(first_data_row(form) - 2):nrow(form_data), first_data_column(form):ncol(form_data)]

  # Might raise the "Warning in FUN(X[[i]], ...) : NAs introduced by coercion" message when catches include non-numeric values...
  records_original = records #[2:nrow(records)]
  records          = records_original[, lapply(.SD, function(value) { return(round(as.numeric(value), 10)) })]

  colnames(records_original) = c("LATITUDE", "LONGITUDE")
  colnames(records)          = c("LATITUDE", "LONGITUDE")

  return(
    list(
      strata = strata,
      records =
        list(
          data = list(
            positions_original = records_original,
            positions          = records
          )
        )
    )
  )
})

setMethod("validate_metadata", list(form = "IOTCForm3BU", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCForm3BU.validate_metadata")

  metadata = form@metadata
  general_information = metadata$general_information

  # Reporting month

  reporting_month_available  = is_provided(general_information$reporting_month)
  reporting_month_valid      = reporting_month_available &&
                               is.integer(general_information$reporting_month) &&
                               general_information$reporting_month %in% 1:12

  common_metadata_validation_results$general_information$reporting_month =
    list(
      available = reporting_month_available,
      value     = general_information$reporting_month,
      valid     = reporting_month_valid
    )

  # Vessel name (optional)

  vessel_name_available  = is_provided(general_information$vessel$name)

  # Vessel IOTC number

  vessel_ID_available    = is_provided(general_information$vessel$IOTC_number)
  vessel_ID_valid        = vessel_ID_available && str_detect(general_information$vessel$IOTC_number, "^IOTC[0-9]{6}$")
  vessel_ID_VRKey        = ifelse(vessel_ID_valid, as.integer(str_replace(general_information$vessel$IOTC_number, "IOTC0+", "")), NA)

  ### Validation

  # Explicitly provided through metadata (although optional)
  vessel_name = general_information$vessel$name

  # Explicitly provided through metadata
  check_flag_country = common_metadata_validation_results$general_information$flag_country
  flag_country = ifelse(check_flag_country$available && check_flag_country$valid,
                        check_flag_country$code, NA)

  # Checks if vessel is mapped to the RAV via its IOTC number / VRVesselKey

  vessel_mapped = !is.na(vessel_ID_VRKey) &&
                  (query(DB_RAV(), paste0("SELECT COUNT(*) FROM [IOTCVessels].[dbo].V_RAV WHERE VRVesselKey = ", vessel_ID_VRKey)) > 0)[[1]] # TRUE / FALSE

  if(vessel_mapped) {
    # Retrieves historical vessel data (name / flag / current) from the RAV

    vessel_data = RAV[IOTC_number == vessel_ID_VRKey]

    vessel_data_current = vessel_data[CURRENT == TRUE]

    current_flag = vessel_data_current$FLAG_CODE
    current_name = vessel_data_current$NAME
  }

  common_metadata_validation_results$general_information$vessel =
    list(
      flag = list(
        available = check_flag_country$available,
        value     = flag_country,
        current   = ifelse(vessel_mapped, current_flag, NA),
        differ    = vessel_mapped && current_flag != flag_country
      ),
      name = list(
        available = vessel_name_available,
        value     = vessel_name,
        current   = ifelse(vessel_mapped, current_name, NA),
        differ    = vessel_mapped && str_to_upper(current_name) != str_to_upper(vessel_name)
      ),
      IOTC_number = list(
        available = vessel_ID_available,
        value     = general_information$vessel$IOTC_number,
        valid     = vessel_ID_valid,
        current   = ifelse(vessel_mapped, vessel_ID_VRKey, NA)
      )
    )

  ## TO BE EXTENDED TO INCLUDE VALIDATION OF REPORTING MONTH AND VESSEL NAME / IOTC NUMBER

  return(common_metadata_validation_results)
})

setGeneric("validate_strata", function(form, strata) {
  standardGeneric("validate_strata")
})

setMethod("validate_strata",
          list(form = "IOTCForm3BU", strata = "data.table"),
          function(form, strata) {
            l_info("IOTCForm3BU.validate_strata")

            reporting_month = form@metadata$general_information$reporting_month

            max_days = fifelse(reporting_month %in% c(4, 6, 9, 11), 30,
                               fifelse(reporting_month %in% c(2), 28,
                                       31))

            strata_empty_rows    = find_empty_rows(strata)
            strata_empty_columns = find_empty_columns(strata)

            strata[, IS_EMPTY := .I %in% strata_empty_rows]
            strata[, OCCURRENCES := .N, by = .(DAY_OF_MONTH, BUOY_ID)]

            valid_strata       = strata[DAY_OF_MONTH %in% 1:max_days & !is.na(BUOY_ID), .(NUM_DAYS = .N), keyby = .(BUOY_ID)]

            total_strata = nrow(strata)

            non_empty_strata = which(strata$IS_EMPTY == FALSE)
            duplicate_strata = which(strata$OCCURRENCES > 1)
            duplicate_strata = duplicate_strata[ ! duplicate_strata %in% strata_empty_rows ]
            unique_strata    = non_empty_strata[ ! non_empty_strata %in% duplicate_strata ]

            incomplete_strata = valid_strata[NUM_DAYS < max_days]
            incomplete_strata = which(strata$BUOY_ID %in% incomplete_strata$BUOY_ID & !is.na(strata$DAY_OF_MONTH))
            #incomplete_strata  = merge(strata, incomplete_strata, all.x = TRUE, sort = FALSE, by = c("BUOY_ID"))

            missing_day_of_month   = which( is.na(strata$DAY_OF_MONTH))
            invalid_day_of_month   = which(strata$DAY_OF_MONTH < 1 | strata$DAY_OF_MONTH > max_days)
            invalid_day_of_month   = invalid_day_of_month[ ! invalid_day_of_month %in% missing_day_of_month ]
            missing_day_of_month   = missing_day_of_month[ ! missing_day_of_month %in% strata_empty_rows]

            missing_buoy_id   = which(is.na(strata$BUOY_ID))
            missing_buoy_id   = missing_buoy_id[ ! missing_buoy_id %in% strata_empty_rows]

            return(
              list(
                empty_rows = list(
                  number      = length(strata_empty_rows),
                  row_indexes = spreadsheet_rows_for(form, strata_empty_rows)
                ),
                empty_columns = list(
                  number      = length(strata_empty_columns),
                  col_indexes = spreadsheet_cols_for(form, strata_empty_columns)
                ),
                total = list(
                  number = total_strata
                ),
                non_empty = list(
                  number = length(non_empty_strata),
                  row_indexes = spreadsheet_rows_for(form, non_empty_strata)
                ),
                duplicate = list(
                  number = length(duplicate_strata),
                  row_indexes = spreadsheet_rows_for(form, duplicate_strata)
                ),
                unique = list(
                  number = length(unique_strata),
                  row_indexes = spreadsheet_rows_for(form, unique_strata)
                ),
                incomplete = list(
                  number = length(incomplete_strata),
                  row_indexes = spreadsheet_rows_for(form, incomplete_strata)
                ),
                checks = list(
                  main = list(
                    day_of_month = list(
                      missing = list(
                        number      = length(missing_day_of_month),
                        row_indexes = spreadsheet_rows_for(form, missing_day_of_month)
                      ),
                      invalid = list(
                        number        = length(invalid_day_of_month),
                        row_indexes   = spreadsheet_rows_for(form, invalid_day_of_month),
                        values        = strata$MONTH[invalid_day_of_month],
                        values_unique = unique(strata$MONTH[invalid_day_of_month])
                      )
                    ),
                    buoy_id = list(
                      missing = list(
                        number = length(missing_buoy_id),
                        row_indexes = spreadsheet_rows_for(form, missing_buoy_id)
                      )
                    )
                  )
                )
              )
            )
})

setMethod("validate_data",
          list(form = "IOTCForm3BU", metadata_validation_results = "list"),
          function(form, metadata_validation_results) {
            l_info("IOTCForm3BU.validate_data")

            reporting_month = form@metadata$general_information$reporting_month

            max_days = fifelse(reporting_month %in% c(4, 6, 9, 11), 30,
                               fifelse(reporting_month %in% c(2), 28,
                                       31))

            strata  = form@data$strata
            records = form@data$records

            data_validation_results = list(
              strata = validate_strata(form, form@data$strata)
            )

            positions_original = records$data$positions_original
            positions          = records$data$positions

            data_empty_rows    = find_empty_rows   (positions)
            data_empty_columns = find_empty_columns(positions)

            numeric_latitude  = is_numeric(positions_original$LATITUDE)
            numeric_longitude = is_numeric(positions_original$LONGITUDE)

            non_num_latitude  = which(numeric_latitude  == FALSE)
            non_num_longitude = which(numeric_longitude == FALSE)

            is_latitude_valid = function(value) {
              return(!is.na(value) & abs(value) < 90)
            }

            is_longitude_valid = function(value) {
              return(!is.na(value) & abs(value) < 180)
            }

            missing_latitude  = which( is.na(positions$LATITUDE))
            invalid_latitude  = which(!is_latitude_valid(positions$LATITUDE))
            invalid_latitude  = invalid_latitude[ ! invalid_latitude %in% missing_latitude ]
            invalid_latitude  = invalid_latitude[ ! invalid_latitude %in% non_num_latitude ]
            missing_latitude  = missing_latitude[ ! missing_latitude %in% data_empty_rows]

            missing_longitude  = which( is.na(positions$LONGITUDE))
            invalid_longitude  = which(!is_longitude_valid(positions$LONGITUDE))
            invalid_longitude  = invalid_longitude[ ! invalid_longitude %in% missing_longitude ]
            invalid_longitude  = invalid_longitude[ ! invalid_longitude %in% non_num_longitude ]
            missing_longitude  = missing_longitude[ ! missing_longitude %in% data_empty_rows]

            data_validation_results$records = list(
              total = nrow(positions),
              empty_rows = list(
                number      = length(data_empty_rows),
                row_indexes = spreadsheet_rows_for(form, data_empty_rows)
              ),
              empty_columns = list(
                number      = length(data_empty_columns),
                col_indexes = spreadsheet_cols_for(form, data_empty_columns)
              ),
              checks = list(
                latitude = list(
                  missing = list(
                    number      = length(missing_latitude),
                    row_indexes = spreadsheet_rows_for(form, missing_latitude)
                  ),
                  non_numeric = list(
                    number       = length(invalid_latitude),
                    row_indexes  = spreadsheet_rows_for(form, non_num_latitude)
                  ),
                  invalid = list(
                    number       = length(invalid_latitude),
                    row_indexes  = spreadsheet_rows_for(form, invalid_latitude)
                  )
                ),
                longitude = list(
                  missing = list(
                    number      = length(missing_longitude),
                    row_indexes = spreadsheet_rows_for(form, missing_longitude)
                  ),
                  non_numeric = list(
                    number       = length(non_num_longitude),
                    row_indexes  = spreadsheet_rows_for(form, non_num_longitude)
                  ),
                  invalid = list(
                    number       = length(invalid_longitude),
                    row_indexes  = spreadsheet_rows_for(form, invalid_longitude)
                  )
                )
              )
            )

            return(data_validation_results)
          }
)

setMethod("metadata_validation_summary", list(form = "IOTCForm3BU", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCForm3BU.metadata_validation_summary")

  validation_messages = new("MessageList")

  general_info = metadata_validation_results$general_information

  # Reference month

  if(!general_info$reporting_month$available)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = "The reporting month is mandatory"))
  else if(!general_info$reporting_month$valid)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The reporting month (", general_info$reporting_month$value, ") is invalid: please use only 1-12 for Jan-Dec")))
  else if(general_info$reporting_year$valid) {
    date = paste0(general_info$reporting_year$value,  '-',
                  str_sub(paste0("00", general_info$reporting_month$value), -2),
                  '-01')

    current_date = format(Sys.time(), "%Y-%m-%d")

    if(date > current_date) {
      validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The reporting year and month identify a date in the future (", date, ")")))
    }
  }

  # Vessel name

  if(!general_info$vessel$name$available)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The vessel name is missing"))

  if(!general_info$vessel$IOTC_number$available) {
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = "The vessel IOTC number (IOTCxxxxxx) is mandatory"))
  } else if(!general_info$vessel$IOTC_number$valid) {
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided vessel IOTC number (", general_info$vessel$IOTC_number$value, ") is incorrect, as it should be in the form 'IOTC' followed by six digits")))
  } else if(is.na(general_info$vessel$IOTC_number$current)) {
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided vessel IOTC number (", general_info$vessel$IOTC_number$value, ") does not identify any valid vessel in the RAV")))
  } else {
    if(general_info$vessel$flag$differ) {
      validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided vessel IOTC number (", general_info$vessel$IOTC_number$value, ") identifies a vessel with a different flag (", general_info$vessel$flag$current, ") than the one provided (", general_info$vessel$flag$value, ")")))
    }

    if(general_info$vessel$name$differ) {
      validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = paste0("The provided vessel IOTC number (", general_info$vessel$IOTC_number$value, ") identifies a vessel with a different name (", general_info$vessel$name$current, ") than the one provided (", general_info$vessel$name$value, ")")))
    }
  }

  return(validation_messages)
})

setMethod("data_validation_summary",
          list(form = "IOTCForm3BU", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCForm3BU.data_validation_summary")

            validation_messages = common_data_validation_summary(form,
                                                                 metadata_validation_results,
                                                                 data_validation_results)

            ### STRATA AND RECORDS

            strata  = data_validation_results$strata
            checks_strata  = strata$checks

            # Strata issues / summary

            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$total$number, " total strata")))
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$non_empty$number, " non-empty strata")))
            validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$unique$number, " unique strata")))

            if(strata$empty_rows$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_rows$number, " empty strata detected: see row(s) #", paste0(strata$empty_rows$row_indexes, collapse = ", "))))

            if(strata$empty_columns$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_columns$number, " empty strata columns detected: see column(s) ", paste0(strata$empty_columns$col_indexes, collapse = ", "))))

            if(strata$duplicate$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

            if(strata$incomplete$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Data is not provided for all days of the month within the strata in row(s) #", paste0(strata$incomplete$row_indexes, collapse = ", "))))

            # Strata checks

            ## Main strata

            checks_strata_main = checks_strata$main

            day_of_month_checks = checks_strata_main$day_of_month

            if(day_of_month_checks$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing day of month in row(s) #", paste0(day_of_month_checks$missing$row_indexes, collapse = ", "))))

            if(day_of_month_checks$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid day of month value in row(s) #", paste0(day_of_month_checks$invalid$row_indexes, collapse = ", "), ". Please use only 1-31 (with upper limit depending on the month of reporting)")))

            buoy_id_checks = checks_strata_main$buoy_id

            if(buoy_id_checks$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing buoy ID in row(s) #", paste0(buoy_id_checks$missing$row_indexes, collapse = ", "))))

            # Data issues / summary

            records = data_validation_results$records
            checks_records = records$checks

            empty_rows = records$empty_rows

            if(empty_rows$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_rows$number, " empty data records detected: see row(s) #", paste0(empty_rows$row_indexes, collapse = ", "))))

            empty_columns = records$empty_columns

            if(empty_columns$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_columns$number, " empty data columns detected: see column(s) ", paste0(empty_columns$col_indexes, collapse = ", "))))

            ## Latitude / longitude

            latitude  = checks_records$latitude
            longitude = checks_records$longitude

            ### Latitude

            if(latitude$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing latitude in row(s) #", paste0(latitude$missing$row_indexes, collapse = ", "))))

            if(latitude$non_numeric$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Non-numeric latitude in row(s) #", paste0(latitude$non_numeric$row_indexes, collapse = ", "))))

            if(latitude$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid latitude in row(s) #", paste0(latitude$invalid$row_indexes, collapse = ", "))))

            ### Longitude

            if(longitude$missing$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing longitude in row(s) #", paste0(longitude$missing$row_indexes, collapse = ", "))))

            if(longitude$non_numeric$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Non-numeric longitude in row(s) #", paste0(longitude$non_numeric$row_indexes, collapse = ", "))))

            if(longitude$invalid$number > 0)
              validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid longitude in row(s) #", paste0(longitude$invalid$row_indexes, collapse = ", "))))

            return(validation_messages)
          }
)

setMethod("common_data_validation_summary",
          list(form = "IOTCForm3BU", metadata_validation_results = "list", data_validation_results = "list"),
          function(form, metadata_validation_results, data_validation_results) {
            l_info("IOTCForm3BU.common_data_validation_summary")

            return(new("MessageList"))
          }
)
