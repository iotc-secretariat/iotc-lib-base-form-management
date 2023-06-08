#' @include Message_class.R
#' @export IOTCForm
IOTCForm = setClass("IOTCForm", representation(path_to_file       = "character",
                                               original_name      = "character",
                                               original_metadata  = "data.table",
                                               original_data      = "data.table",
                                               metadata           = "list",
                                               data               = "list"),
                                prototype     (original_name     = NA_character_,
                                               original_metadata = as.data.table(NA),
                                               original_data     = as.data.table(NA)))

setGeneric("validate_type_and_version", function(form) {
  standardGeneric("validate_type_and_version")
})

# Doesn't need to be extended
setMethod("validate_type_and_version", "IOTCForm", function(form) {
  form_type    = form@metadata$form_details$type
  form_version = form@metadata$form_details$version

  if(check_mandatory(form_type, "Form type") != form_type(form))
    stop(call. = FALSE, paste0("Please provide a valid form ", form_type(form), " (current form type: ", form_type, " - required: ", form_type(form), ")"))

  if(check_mandatory(form_version, "Form version") != form_version(form))
    stop(call. = FALSE, paste0("Please provide a valid form ", form_type(form), " (current form version: ", form_version, " - required: ", form_version(form), ")"))
})

setGeneric("read", function(form) {
  standardGeneric("read")
})

# Doesn't need to be extended
setMethod("read", "IOTCForm", function(form) {
  start = Sys.time()
  l_info("IOTCForm.read")

  current_form = read_form(form@path_to_file, form@original_name)

  form@original_metadata = current_form$form_metadata
  form@original_data     = current_form$form_data

  common_metadata          = common_metadata(form@original_metadata)
  common_metadata$comments = comments(form@original_metadata, form_comment_cell_row(form))

  form@metadata = extract_metadata(form, common_metadata)

  validate_type_and_version(form)

  form@data = extract_data(form)

  l_debug(paste0("IOTCForm.read: ", Sys.time() - start))

  return(form)
})

setGeneric("validate_common_metadata", function(form) {
  standardGeneric("validate_common_metadata")
})

setMethod("validate_common_metadata", "IOTCForm", function(form) {
  start = Sys.time()
  l_info("IOTCForm.validate_common_metadata")

  metadata = form@metadata

  submission_information = metadata$submission_information
  general_information    = metadata$general_information

  focal_point     = submission_information$focal_point
  organization    = submission_information$organization

  focal_point_full_name_available = is_provided(focal_point$full_name)
  focal_point_e_mail_available    = is_provided(focal_point$e_mail)

  organization_full_name_available = is_provided(organization$full_name)
  organization_e_mail_available    = is_provided(organization$e_mail)

  reference_dates = submission_information$reference_dates

  reference_dates_finalization_available = is_provided(reference_dates$finalization)
  reference_dates_submission_available   = is_provided(reference_dates$submission)

  reference_dates_finalization_valid = reference_dates_finalization_available && reference_dates$finalization <= format(Sys.time())
  reference_dates_submission_valid   = reference_dates_submission_available   && reference_dates$submission   <= format(Sys.time())

  reference_dates_coherent =
    reference_dates_finalization_valid &
    reference_dates_submission_valid &
    reference_dates$submission >= reference_dates$finalization

  reporting_year_available   = is_provided(general_information$reporting_year)
  reporting_entity_available = is_provided(general_information$reporting_entity)
  flag_country_available     = is_provided(general_information$flag_country)

  reporting_year_valid   = reporting_year_available && is_year_valid(general_information$reporting_year)
  reporting_entity_valid = reporting_entity_available && is_entity_valid(general_information$reporting_entity)
  flag_country_valid     = flag_country_available && is_country_valid(general_information$flag_country)

  fleet_valid =
    reporting_entity_valid &&
    flag_country_valid &&
    is_fleet_valid(general_information$reporting_entity,
                   general_information$flag_country)

  fleet = NA

  if(fleet_valid)
    fleet = fleets_for(general_information$reporting_entity,
                       general_information$flag_country)

  l_debug(paste0("IOTCForm.validate_common_metadata: ", Sys.time() - start))

  return(
    list(
      submission_information = list(
        focal_point = list(
          available = list(
            full_name = focal_point_full_name_available,
            e_mail    = focal_point_e_mail_available
          )
        ),
        organization = list(
          available = list(
            full_name = organization_full_name_available,
            e_mail    = organization_e_mail_available
          )
        ),
        reference_dates = list(
          finalization = list(
            available = is_provided(reference_dates$finalization),
            value     = reference_dates$finalization,
            valid     = reference_dates_finalization_valid
          ),
          submission = list(
            available = is_provided(reference_dates$submission),
            value     = reference_dates$submission,
            valid     = reference_dates_submission_valid
          ),
          checks = list(
            dates_are_coherent = reference_dates_coherent
          )
        )
      ),
      general_information = list(
        reporting_year = list(
          available = reporting_year_available,
          value     = general_information$reporting_year,
          valid     = reporting_year_valid
        ),
        reporting_entity = list(
          available = reporting_entity_available,
          code      = general_information$reporting_entity,
          valid     = reporting_entity_valid
        ),
        flag_country = list(
          available = flag_country_available,
          code      = general_information$flag_country,
          valid     = flag_country_valid
        ),
        fleet = list(
          valid     = fleet_valid,
          code      = ifelse(fleet_valid, fleet$FLEET_CODE, NA),
          name      = ifelse(fleet_valid, fleet$NAME_EN, NA)
        )
      )
    )
  )
})

setGeneric("validate", function(form) {
  standardGeneric("validate")
})

setMethod("validate", "IOTCForm", function(form) {
  start = Sys.time()
  l_info("IOTCForm.validate")

  metadata_validation = validate_metadata(form, validate_common_metadata(form))
  data_validation     = validate_data    (form, metadata_validation)

  l_debug(paste0("IOTCForm.validate: ", Sys.time() - start))

  return(
    list(
      metadata = metadata_validation,
      data     = data_validation
    )
  )
})

setGeneric("common_metadata_validation_summary", function(form, metadata_validation_results) {
  standardGeneric("common_metadata_validation_summary")
})

setMethod("common_metadata_validation_summary", signature(form = "IOTCForm", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start = Sys.time()
  l_info("IOTCForm.common_metadata_validation_summary")

  validation_messages = new("MessageList")

  submission_information = metadata_validation_results$submission_information
  general_information    = metadata_validation_results$general_information

  # Submission information

  ## Focal point

  if(!submission_information$focal_point$available$full_name)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The focal point full name is mandatory"))

  if(!submission_information$focal_point$available$e_mail)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The focal point e-mail is mandatory"))

  ## Organization

  if(!submission_information$organization$available$full_name)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The organization name is mandatory"))

  if(!submission_information$organization$available$e_mail)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Metadata", text = "The organization e-mail is not available"))

  ## Reference dates

  if(!submission_information$reference_dates$finalization$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The finalization date is mandatory"))
  else if(!submission_information$reference_dates$finalization$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The finalization date (", submission_information$reference_dates$finalization$value, ") is not valid or is set in the future")))

  if(!submission_information$reference_dates$submission$available)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = "The submission date is mandatory"))
  else if(!submission_information$reference_dates$submission$valid)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The submission date (", submission_information$reference_dates$submission$value, ") is not valid or is set in the future")))

  if( submission_information$reference_dates$finalization$valid &&
      submission_information$reference_dates$submission$valid &&
      !submission_information$reference_dates$checks$dates_are_coherent)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Metadata", text = paste0("The submission date (", submission_information$reference_dates$submission$value, ") should follow the finalization date (", submission_information$reference_dates$finalization$value, ")")))

  # General information

  ## Reporting year

  if(!general_information$reporting_year$available)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = "The reporting year is mandatory"))
  else if(!general_information$reporting_year$valid)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The reporting year (", general_information$reporting_year$value, ") must not be in the future")))

  ## Reporting entity

  if(!general_information$reporting_entity$available)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = "The reporting entity is mandatory"))
  else if(!general_information$reporting_entity$valid)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") is not valid. Please refer to ", reference_codes("admin", "entities"), " for a list of valid entity codes")))

  ## Flag country

  if(!general_information$flag_country$available)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = "The flag country is mandatory"))
  else if(!general_information$flag_country$valid)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided flag country (", general_information$flag_country$code, ") is not valid. Please refer to ", reference_codes("admin", "countries"), " for a list of valid country codes")))

  if( general_information$reporting_entity$valid &
      general_information$flag_country$valid &
     !general_information$fleet$valid)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") and flag country (", general_information$flag_country$code, ") do not identify any valid fleet")))

  if(general_information$fleet$valid)
    validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Metadata", text = paste0("The provided reporting entity (", general_information$reporting_entity$code, ") and flag country (", general_information$flag_country$code, ") identify ", general_information$fleet$code, " ('", general_information$fleet$name, "') as fleet")))

  l_debug(paste0("IOTCForm.common_metadata_validation_summary: ", Sys.time() - start))

  return(validation_messages)
})

#' @export
setGeneric("validation_summary", function(form) {
  standardGeneric("validation_summary")
})

# Doesn't need to be extended
setMethod("validation_summary", "IOTCForm", function(form) {
  start = Sys.time()
  l_info("IOTCForm.validation_summary")

  validation_messages = new("MessageList")

  validation_results = NULL

  tryCatch({
    current_form = read(form)

    validation_results = validate(current_form)
  }, error = function(cond) {
    l_error(cond)

    validation_messages <<- add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("Undetected bug encountered while validating form! Please report this message to IOTC-Statistics@fao.org, including a copy of the file you uploaded: ", cond$message)))
  })

  tryCatch({ # Unfortunately this doesn't seem to work with some type of errors, which are de-facto uncatchable
    if(!is.null(validation_results)) {
      metadata_validation_results = validation_results$metadata
      data_validation_results     = validation_results$data

      common_metadata_validation_messages = common_metadata_validation_summary(form, metadata_validation_results)
      metadata_validation_messages        = metadata_validation_summary       (form, metadata_validation_results)
      data_validation_messages            = data_validation_summary           (form, metadata_validation_results, data_validation_results)

      all_validation_messages = rbind(common_metadata_validation_messages@messages,
                                      metadata_validation_messages@messages,
                                      data_validation_messages@messages)
    } else {
      all_validation_messages = validation_messages@messages
    }
  }, error = function(cond) {
    l_error(cond)

    all_validation_messages <<- add(validation_messages, new("Message", level = "FATAL", source = "Metadata", text = paste0("Undetected bug encountered while producing validation summary for the form! Please report this message to IOTC-Statistics@fao.org, including a copy of the file you uploaded: ", cond$message)))
  })

  info_messages    = all_validation_messages[LEVEL == "INFO"]
  warning_messages = all_validation_messages[LEVEL == "WARN"]
  error_messages   = all_validation_messages[LEVEL == "ERROR"]
  fatal_messages   = all_validation_messages[LEVEL == "FATAL"]

  msg_info    = nrow(info_messages)
  msg_warn    = nrow(warning_messages)
  msg_error   = nrow(error_messages)
  msg_fatal   = nrow(fatal_messages)

  summary =
    ifelse(
      msg_fatal > 0, "Fatal issues encountered: check file consistency with respect to official IOTC forms, ensure that all metadata are correct, and verify that no empty or duplicate strata is found in the 'Data' worksheet",
      ifelse(msg_error > 0, "Errors encountered: check that all codes correspond to official reference codes and that the semantics of the provided data is enforced",
             ifelse(msg_warn > 0, "Warnings encountered: the file can be processed, but it is recommended to identify and fix the causes of the highlighted warnings",
                    "The file can be successfully processed"
             )
      )
    )

  l_debug(paste0("IOTCForm.validation_summary: ", Sys.time() - start))

  return(
    list(
      summary             = summary,
      can_be_processed    = msg_fatal == 0 && msg_error == 0,
      info_messages       = msg_info,
      warning_messages    = msg_warn,
      error_messages      = msg_error,
      fatal_messages      = msg_fatal,
      validation_messages = all_validation_messages
    )
  )
})

#' @export
setGeneric("spreadsheet_rows_for", function(form, row_indexes) {
  standardGeneric("spreadsheet_rows_for")
})

# Doesn't need to be extended
setMethod("spreadsheet_rows_for", signature(form = "IOTCForm", row_indexes = "numeric"), function(form, row_indexes) {
  first_row = first_data_row(form)

  return(
    as.integer(first_row - 1 + row_indexes)
  )
})

#' @export
setGeneric("spreadsheet_cols_for", function(form, col_indexes) {
  standardGeneric("spreadsheet_cols_for")
})

# Doesn't need to be extended
setMethod("spreadsheet_cols_for", signature(form = "IOTCForm", col_indexes = "numeric"), function(form, col_indexes) {
  first_col = first_data_column(form)

  return(
    EXCEL_COLUMNS[first_col - 1 + col_indexes]
  )
})

### METHODS TO HANDLE VALIDATION SUMMARY FOR COMMON ELEMENTS

# STRATA

setGeneric("report_strata", function(form, message_list, strata_validation) {
  standardGeneric("report_strata")
})

setMethod("report_strata", signature(form = "IOTCForm", message_list = "MessageList",
                                     strata_validation = "list"), function(form, message_list, strata_validation) {

  # Generic INFO messages on strata composition

  message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$total$number,     " total strata"    )))
  message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$non_empty$number, " non-empty strata")))
  message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$unique$number,    " unique strata"   )))

  # FATAL messages on empty rows

  if(strata_validation$empty_rows$number > 0) {
    if(strata_validation$empty_rows$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(strata_validation$empty_rows$number, " empty strata detected")))

    for(row in strata_validation$empty_rows$row_indexes)
      message_list = add(message_list, new("Message", level = "FATAL", source = "Data", row = row, text = paste0("Empty stratum detected at row #", row)))
  }

  # FATAL messages on empty columns

  if(strata_validation$empty_columns$number > 0) {
    if(strata_validation$empty_columns$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(strata_validation$empty_columns$number, " empty strata columns detected")))

    for(col in strata_validation$empty_columns$col_indexes)
      message_list = add(message_list, new("Message", level = "FATAL", source = "Data", column = col, text = paste0("Empty stratum column detected at column ", col)))
  }

  # FATAL messages on duplicate strata

  if(strata_validation$duplicate$number > 0)
    message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(strata_validation$duplicate$number, " duplicate strata detected: see row(s) #", paste0(strata_validation$duplicate$row_indexes, collapse = ", "))))

  return(message_list)
})

# DATA

setGeneric("report_data", function(form, message_list, data_validation, allow_empty_records) {
  standardGeneric("report_data")
})

setMethod("report_data", signature(form = "IOTCForm", message_list = "MessageList",
          data_validation = "list", allow_empty_records = "logical"), function(form, message_list, data_validation, allow_empty_records) {
  # Generic INFO messages on data composition

  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$total$number,     " total strata"    )))
  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$non_empty$number, " non-empty strata")))
  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$unique$number,    " unique strata"   )))

  # FATAL messages on empty rows

  if(data_validation$empty_rows$number > 0 && !allow_empty_records) {
    if(data_validation$empty_rows$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(data_validation$empty_rows$number, " empty data records detected")))

    for(row in data_validation$empty_rows$row_indexes) {
      message_list = add(message_list, new("Message", level = "FATAL", source = "Data", row = row, text = paste0("Empty data record at row #", row)))
    }
  }

  # FATAL messages on empty columns

  if(data_validation$empty_columns$number > 0) {
    if(data_validation$empty_columns$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(data_validation$empty_columns$number, " empty data columns detected")))

    for(col in data_validation$empty_columns$col_indexes) {
      message_list = add(message_list, new("Message", level = "FATAL", source = "Data", column = col, text = paste0(data_validation$empty_columns$number, "Empty data column detected at column ", col)))
    }
  }

  return(message_list)
 })

# FISHERIES

setGeneric("report_fisheries", function(form, message_list, fisheries_validation, column) {
  standardGeneric("report_fisheries")
})

setMethod("report_fisheries", signature(form = "IOTCForm", message_list = "MessageList",
                                                fisheries_validation = "list", column = "character"), function(form, message_list, fisheries_validation, column) {
    if(fisheries_validation$aggregates$number > 0) {
      message_list = add(message_list, new("Message", level = "WARN", source = "Data", column = column, text = paste0(fisheries_validation$aggregates$number, " aggregated fishery code(s) reported. Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid legacy fishery codes")))

      for(row in fisheries_validation$aggregates$row_indexes) {
        message_list = add(message_list, new("Message", level = "WARN", source = "Data", column = column, row = row, text = paste0("Aggregated fishery code reported in row #", row)))
      }
    }

    if(fisheries_validation$missing$number > 0) {
      if(fisheries_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(fisheries_validation$missing$number, " missing fishery codes")))

      for(row in fisheries_validation$missing$row_indexes) {
        message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing fishery code in row #", row)))
      }
    }

    if(fisheries_validation$invalid$number > 0) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(fisheries_validation$invalid$number, " invalid fishery code(s) reported. Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid legacy fishery codes")))

      for(row in fisheries_validation$invalid$row_indexes) {
        message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid fishery code in row #", row)))
      }
    }

    return(message_list)
})

# TARGET SPECIES

setGeneric("report_target_species", function(form, message_list, target_species_validation, column) {
  standardGeneric("report_target_species")
})

setMethod("report_target_species", signature(form = "IOTCForm", message_list = "MessageList",
                                        target_species_validation = "list", column = "character"), function(form, message_list, target_species_validation, column) {

  if(target_species_validation$missing$number > 0) {
    if(target_species_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(target_species_validation$missing$number, " missing target species code(s)")))

    for(row in target_species_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing target species code in row #", row)))
    }
  }

  if(target_species_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(target_species_validation$invalid$number, " invalid target species code(s) reported. Please refer to ", reference_codes("legacy", "species"), " for a list of valid legacy fishery codes")))

    for(row in target_species_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid target species code in row #", row)))
    }
  }

  return(message_list)
})

# IOTC AREA

setGeneric("report_IOTC_area", function(form, message_list, area_validation, column) {
  standardGeneric("report_IOTC_area")
})

setMethod("report_IOTC_area", signature(form = "IOTCForm", message_list = "MessageList",
                                        area_validation = "list", column = "character"), function(form, message_list, area_validation, column) {
  if(area_validation$missing$number > 0) {
    if(area_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(area_validation$missing$number, " missing IOTC main area codes")))

    for(row in area_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing IOTC main area code in row #", row)))
    }
  }

  if(area_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(area_validation$invalid$number, " invalid IOTC main area code(s) reported. Please refer to ", reference_codes("admin", "IOTCareasMain"), " for a list of valid IOTC main area codes")))

    for(row in area_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid IOTC main area code in row #", row)))
    }
  }

  return(message_list)
})

# DATA TYPE

setGeneric("report_data_type", function(form, message_list, data_type_validation, column) {
  standardGeneric("report_data_type")
})

setMethod("report_data_type", signature(form = "IOTCForm", message_list = "MessageList",
                                        data_type_validation = "list", column = "character"), function(form, message_list, data_type_validation, column) {
  if(data_type_validation$missing$number > 0) {
    if(data_type_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_type_validation$missing$number, " missing data type codes")))

    for(row in data_type_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data type code in row #", row)))
    }
  }

  if(data_type_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_type_validation$invalid$number, " invalid data type code(s) reported. Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

    for(row in data_type_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data type code in row #", row)))
    }
  }

  return(message_list)
})

# DATA SOURCE

setGeneric("report_data_source", function(form, message_list, data_source_validation, column) {
  standardGeneric("report_data_source")
})

setMethod("report_data_source", signature(form = "IOTCForm", message_list = "MessageList",
                                          data_source_validation = "list", column = "character"), function(form, message_list, data_source_validation, column) {
  if(data_source_validation$missing$number > 0) {
    if(data_source_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_source_validation$missing$number, " missing data source codes")))

    for(row in data_source_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data source code in row #", row)))
    }
  }

  if(data_source_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_source_validation$invalid$number, " invalid data source code(s) reported. Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes for this dataset")))

    for(row in data_source_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data source code in row #", row)))
    }
  }

  return(message_list)
})

# DATA PROCESSING

setGeneric("report_data_processing", function(form, message_list, data_processing_validation, column) {
  standardGeneric("report_data_processing")
})

setMethod("report_data_processing", signature(form = "IOTCForm", message_list = "MessageList",
                                              data_processing_validation = "list", column = "character"), function(form, message_list, data_processing_validation, column) {
  if(data_processing_validation$missing$number > 0) {
    if(data_processing_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_processing_validation$missing$number, " missing data processing codes")))

    for(row in data_processing_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data processing code in row #", row)))
    }
  }

  if(data_processing_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_processing_validation$invalid$number, " invalid data processing code(s) reported. Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes for this dataset")))

    for(row in data_processing_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data processing code in row #", row)))
    }
  }

  return(message_list)
})

# COVERAGE TYPE

setGeneric("report_coverage_type", function(form, message_list, coverage_type_validation, column) {
  standardGeneric("report_coverage_type")
})

setMethod("report_coverage_type", signature(form = "IOTCForm", message_list = "MessageList",
                                            coverage_type_validation = "list", column = "character"), function(form, message_list, coverage_type_validation, column) {
  if(coverage_type_validation$missing$number > 0) {
    if(coverage_type_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_type_validation$missing$number, " missing coverage type codes")))

    for(row in coverage_type_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing coverage type code in row #", row)))
    }
  }

  if(coverage_type_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_type_validation$invalid$number, " invalid coverage type code(s) reported. Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid data coverage types")))

    for(row in coverage_type_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid coverage type code in row #", row)))
    }
  }

  return(message_list)
})

# COVERAGE VALUE

setGeneric("report_coverage_value", function(form, message_list, coverage_value_validation, column) {
  standardGeneric("report_coverage_value")
})

setMethod("report_coverage_value", signature(form = "IOTCForm", message_list = "MessageList",
                                             coverage_value_validation = "list", column = "character"), function(form, message_list, coverage_value_validation, column) {
  if(coverage_value_validation$missing$number > 0) {
    if(coverage_value_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_value_validation$missing$number, " missing coverage values")))

    for(row in coverage_value_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing coverage value in row #", row)))
    }
  }

  if(coverage_value_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_value_validation$invalid$number, " invalid coverage value(s) reported")))

    for(row in coverage_value_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid coverage value in row #", row)))
    }
  }

  return(message_list)
})

### METHODS TO BE IMPLEMENTED BY SUBCLASSES

setGeneric("form_type", function(form) {
  standardGeneric("form_type")
})

setGeneric("form_version", function(form) {
  standardGeneric("form_version")
})

setGeneric("form_comment_cell_row", function(form) {
  standardGeneric("form_comment_cell_row")
})

setGeneric("form_dataset_code", function(form) {
  standardGeneric("form_dataset_code")
})

## METADATA

setGeneric("extract_metadata", function(form, common_metadata) {
  standardGeneric("extract_metadata")
})

setGeneric("validate_metadata", function(form, common_metadata_validation_results) {
  standardGeneric("validate_metadata")
})

setGeneric("metadata_validation_summary", function(form, metadata_validation_results) {
  standardGeneric("metadata_validation_summary")
})

## DATA

setGeneric("first_data_column", function(form) {
  standardGeneric("first_data_column")
})

setGeneric("first_data_row", function(form) {
  standardGeneric("first_data_row")
})

setGeneric("first_strata_column", function(form) {
  standardGeneric("first_strata_column")
})

setGeneric("last_strata_column", function(form) {
  standardGeneric("last_strata_column")
})

setGeneric("extract_data", function(form) {
  standardGeneric("extract_data")
})

setGeneric("validate_data", function(form, metadata_validation_results) {
  standardGeneric("validate_data")
})

setGeneric("common_data_validation_summary", function(form, metadata_validation_results, data_validation_results) {
  standardGeneric("common_data_validation_summary")
})

setGeneric("data_validation_summary", function(form, metadata_validation_results, data_validation_results) {
  standardGeneric("data_validation_summary")
})
