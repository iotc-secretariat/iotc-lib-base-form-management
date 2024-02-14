#' @include IOTC_form_class.R
#' @export IOTCFormCESF
IOTCFormCESF = setClass(
  "IOTCFormCESF",
  contains = "IOTCForm"
)

setGeneric("grid_validator", function(form) {
  standardGeneric("grid_validator")
})

setGeneric("allow_empty_data", function(form) {
  standardGeneric("allow_empty_data")
})

setGeneric("estimation_column", function(form) {
  standardGeneric("estimation_column")
})

setGeneric("optional_strata_columns", function(form) {
  standardGeneric("optional_strata_columns")
})

# Renamed to avoid (UNEXPLAINED!) issues with inheritance... Probably the same should happen for "allow_empty_data" and "grid_validator"
# which have the same name as methods in IOTCFormCESF
setGeneric("validate_months", function(form, strata) {
  standardGeneric("validate_months")
})

setMethod("form_comment_cell_row", "IOTCFormCESF", function(form) {
  return(23) #Default for CE / SF (multiple)
})

setMethod("extract_metadata", list(form = "IOTCFormCESF", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCFormCESF.extract_metadata")
  return(common_metadata)
})

setMethod("validate_metadata", list(form = "IOTCFormCESF", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCFormCESF.validate_metadata")

  return(common_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCFormCESF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESF.metadata_validation_summary")

  return(new("MessageList"))
})

setMethod("validate_data", list(form = "IOTCFormCESF", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  start_VDD = Sys.time()
  l_info("IOTCFormCESF.validate_data")

  strata_orig = form@data$strata
  strata_orig$MONTH = strata_orig$MONTH_ORIGINAL
  strata_orig$MONTH_ORIGINAL  = NULL

  strata  = form@data$strata
  records = form@data$records

  data_CE_SF_original = records$data$CE_SF_data_original
  data_CE_SF          = records$data$CE_SF_data

  strata_empty_rows    = find_empty_rows(strata_orig)
  strata_empty_columns = find_empty_columns(strata_orig)
  strata_empty_columns = strata_empty_columns[which(!strata_empty_columns %in% optional_strata_columns(form))]

  strata[, IS_EMPTY := .I %in% strata_empty_rows]

  # Adds all other metadata associated to the fishery
  strata = merge(strata, iotc.data.reference.codelists::FISHERIES[, .(CODE, FISHERY_CATEGORY_CODE)],
                 by.x = "FISHERY_CODE", by.y = "CODE",
                 all.x = TRUE, sort = FALSE)

  total_strata     = nrow(strata)
  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (I): ", Sys.time() - start_VDD))
  start_VD = Sys.time()

  data_empty_rows    = find_empty_rows(data_CE_SF)
  data_empty_columns = find_empty_columns(data_CE_SF)

  l_debug(paste0("IOTCFormCESF.validate_data (II): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_months   = which( is.na(strata$MONTH_ORIGINAL))
  invalid_months   = which(!is_month_valid(strata$MONTH_ORIGINAL))

  invalid_months   = invalid_months[ ! invalid_months %in% missing_months ]
  missing_months   = missing_months[ ! missing_months %in% strata_empty_rows]

  l_debug(paste0("IOTCFormCESF.validate_data (III): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  # If all months are provided and valid, we check that they're also consistent...

  months_check = validate_months(form, strata)

  l_debug(paste0("IOTCFormCESF.validate_data (IV): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_fisheries  = which( is.na(strata$FISHERY_CODE))
  invalid_fisheries  = which(!is_fishery_valid(strata$FISHERY_CODE))

  invalid_fisheries  = invalid_fisheries[ ! invalid_fisheries %in% missing_fisheries ]
  missing_fisheries  = missing_fisheries[ ! missing_fisheries %in% strata_empty_rows]

  l_debug(paste0("IOTCFormCESF.validate_data (V): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  l_debug(paste0("IOTCFormCESF.validate_data (VIa): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  fishery_types = fishery_type_for(strata$FISHERY_CODE)

  l_debug(paste0("IOTCFormCESF.validate_data (VIb): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  fishery_aggregates = which(is_multiple_gear_fishery(strata$FISHERY_CODE))

  l_debug(paste0("IOTCFormCESF.validate_data (VII): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_grids = which( is.na(strata$GRID_CODE))
  invalid_grids = which(!grid_validator(form)(strata$GRID_CODE))
  invalid_grids = invalid_grids[ ! invalid_grids %in% missing_grids ]
  missing_grids = missing_grids[ ! missing_grids %in% strata_empty_rows ]

  valid_grids   = strata$GRID_CODE
  valid_grids   = which(!is.na(strata$GRID_CODE))

  l_debug(paste0("IOTCFormCESF.validate_data (VIII): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_estimations    = which( is.na(strata$ESTIMATION_CODE))
  invalid_estimations    = which(!is_data_estimation_valid(strata$ESTIMATION_CODE))
  invalid_estimations    = invalid_estimations[ ! invalid_estimations %in% missing_estimations ]
  missing_estimations    = missing_estimations[ ! missing_estimations %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (IX): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_types_of_data    = which( is.na(strata$DATA_TYPE_CODE))
  invalid_types_of_data    = which(!is_data_type_valid(strata$DATA_TYPE_CODE))
  invalid_types_of_data    = invalid_types_of_data[ ! invalid_types_of_data %in% missing_types_of_data ]
  missing_types_of_data    = missing_types_of_data[ ! missing_types_of_data %in% strata_empty_rows ]

  # 'PR'-eliminary data is only expected to be reported for LONGLINE fisheries
  wrong_types_of_data      = which(is_data_type_valid(strata$DATA_TYPE_CODE) &
                                   strata$FISHERY_CATEGORY_CODE != "LONGLINE" &
                                   strata$DATA_TYPE_CODE == "PR")

  unknown_types_of_data    = which(strata$DATA_TYPE_CODE == "UN")

  l_debug(paste0("IOTCFormCESF.validate_data (X): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  form_dataset = form_dataset_code(form)

  missing_data_sources     = which( is.na(strata$DATA_SOURCE_CODE))
  invalid_data_sources     = which(!is.na(strata$DATA_SOURCE_CODE) & !is_data_source_valid(form_dataset, strata$DATA_SOURCE_CODE))
  invalid_data_sources     = invalid_data_sources[ ! invalid_data_sources %in% missing_data_sources ]
  missing_data_sources     = missing_data_sources[ ! missing_data_sources %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (XI): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_data_processings = which( is.na(strata$DATA_PROCESSING_CODE))
  invalid_data_processings = which(!is.na(strata$DATA_PROCESSING_CODE) & !is_data_processing_valid(form_dataset, strata$DATA_PROCESSING_CODE))
  invalid_data_processings = invalid_data_processings[ ! invalid_data_processings %in% missing_data_processings ]
  missing_data_processings = missing_data_processings[ ! missing_data_processings %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (XII): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_data_raisings    = which( is.na(strata$DATA_RAISING_CODE))
  invalid_data_raisings    = which(!is_data_raising_valid(strata$DATA_RAISING_CODE))
  invalid_data_raisings    = invalid_data_raisings[ ! invalid_data_raisings %in% missing_data_raisings ]
  missing_data_raisings    = missing_data_raisings[ ! missing_data_raisings %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (XIII): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  missing_coverage_types   = which( is.na(strata$COVERAGE_TYPE_CODE))
  invalid_coverage_types   = which(!is_data_coverage_type_valid(strata$COVERAGE_TYPE_CODE))
  invalid_coverage_types   = invalid_coverage_types[ ! invalid_coverage_types %in% missing_coverage_types ]
  missing_coverage_types   = missing_coverage_types[ ! missing_coverage_types %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (XIV): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  # Temporarily added
  strata$COVERAGE = as.double(strata$COVERAGE)

  missing_coverages        = which( is.na(strata$COVERAGE))
  invalid_coverages        = which(!is_percentage_valid(strata$COVERAGE))
  invalid_coverages        = invalid_coverages[ ! invalid_coverages %in% missing_coverages ]
  missing_coverages        = missing_coverages[ ! missing_coverages %in% strata_empty_rows ]

  l_debug(paste0("IOTCFormCESF.validate_data (XV): ", Sys.time() - start_VD))
  start_VD = Sys.time()

  l_debug(paste0("IOTCFormCESF.validate_data: ", Sys.time() - start_VDD))

  return(
    list(
      strata = list(
        empty_rows = list(
          number      = length(strata_empty_rows),
          row_indexes = spreadsheet_rows_for(form, strata_empty_rows)
        ),
        empty_columns = list(
          number      = length(strata_empty_columns),
          col_indexes = spreadsheet_cols_for_strata(form, strata_empty_columns)
        ),
        total = list(
          number = total_strata
        ),
        non_empty = list(
          number = length(non_empty_strata),
          row_indexes = spreadsheet_rows_for(form, non_empty_strata)
        ),
        checks = list(
          main = list(
            months = list(
              missing = list(
                number      = length(missing_months),
                row_indexes = spreadsheet_rows_for(form, missing_months)
              ),
              invalid = list(
                number        = length(invalid_months),
                row_indexes   = spreadsheet_rows_for(form, invalid_months),
                values        = strata$MONTH[invalid_months],
                values_unique = unique(strata$MONTH[invalid_months])
              )
              # REMOVED: while we expect data to be provided for all quarters in 1-RC and 1-DI, same is not the case for 3-CE or 4-SF where stratification is much finer
              #, incomplete = list(
              #  number      = length(months_check$incomplete_months),
              #  row_indexes = spreadsheet_rows_for(form, months_check$incomplete_months
              #)
            ),
            fisheries = list(
              invalid = list(
                number       = length(invalid_fisheries),
                row_indexes  = spreadsheet_rows_for(form, invalid_fisheries),
                codes        = strata$FISHERY_CODE[invalid_fisheries],
                codes_unique = unique(strata$FISHERY_CODE[invalid_fisheries])
              ),
              missing = list(
                number      = length(missing_fisheries),
                row_indexes = spreadsheet_rows_for(form, missing_fisheries)
              ),
              aggregates = list(
                number       = length(fishery_aggregates),
                row_indexes  = spreadsheet_rows_for(form, fishery_aggregates),
                codes        = strata[fishery_aggregates]$FISHERY_CODE,
                codes_unique = unique(strata[fishery_aggregates]$FISHERY_CODE)
              ),
              types = list(
                artisanal = list(
                  number      = length(which(fishery_types == "AR")),
                  row_indexes = spreadsheet_rows_for(form, which(fishery_types == "AR"))
                ),
                semi_industrial = list(
                  number      = length(which(fishery_types == "SI")),
                  row_indexes = spreadsheet_rows_for(form, which(fishery_types == "SI"))
                ),
                industrial = list(
                  number      = length(which(fishery_types == "IN")),
                  row_indexes = spreadsheet_rows_for(form, which(fishery_types == "IN"))
                )
              )
            ),
            grids = list(
              invalid = list(
                number       = length(invalid_grids),
                row_indexes  = spreadsheet_rows_for(form, invalid_grids),
                codes        = strata[invalid_grids]$GRID_CODE,
                codes_unique = unique(strata[invalid_grids]$GRID_CODE)
              ),
              missing = list(
                number      = length(missing_grids),
                row_indexes = spreadsheet_rows_for(form, missing_grids)
              )
            ),
            estimations = list(
              invalid = list(
                number       = length(invalid_estimations),
                row_indexes  = spreadsheet_rows_for(form, invalid_estimations),
                codes        = strata[invalid_estimations]$ESTIMATION_CODE,
                codes_unique = unique(strata[invalid_estimations]$ESTIMATION_CODE)
              ),
              missing = list(
                number      = length(missing_estimations),
                row_indexes = spreadsheet_rows_for(form, missing_estimations)
              )
            )
          ),
          original_data = list(
            type = list(
              invalid = list(
                number       = length(invalid_types_of_data),
                row_indexes  = spreadsheet_rows_for(form, invalid_types_of_data),
                codes        = strata[invalid_types_of_data]$DATA_TYPE_CODE,
                codes_unique = unique(strata[invalid_types_of_data]$DATA_TYPE_CODE)
              ),
              missing = list(
                number      = length(missing_types_of_data),
                row_indexes = spreadsheet_rows_for(form, missing_types_of_data)
              ),
              wrong = list(
                number      = length(wrong_types_of_data),
                row_indexes = spreadsheet_rows_for(form, wrong_types_of_data)
              ),
              unknown = list(
                number      = length(unknown_types_of_data),
                row_indexes = spreadsheet_rows_for(form, unknown_types_of_data)
              )
            ),
            source = list(
              invalid = list(
                number       = length(invalid_data_sources),
                row_indexes  = spreadsheet_rows_for(form, invalid_data_sources),
                codes        = strata[invalid_data_sources]$DATA_SOURCE_CODE,
                codes_unique = unique(strata[invalid_data_sources]$DATA_SOURCE_CODE)
              ),
              missing = list(
                number      = length(missing_data_sources),
                row_indexes = spreadsheet_rows_for(form, missing_data_sources)
              )
            ),
            processing = list(
              invalid = list(
                number       = length(invalid_data_processings),
                row_indexes  = spreadsheet_rows_for(form, invalid_data_processings),
                codes        = strata[invalid_data_processings]$DATA_PROCESSING_CODE,
                codes_unique = unique(strata[invalid_data_processings]$DATA_PROCESSING_CODE)
              ),
              missing = list(
                number      = length(missing_data_processings),
                row_indexes = spreadsheet_rows_for(form, missing_data_processings)
              )
            ),
            raising = list(
              invalid = list(
                number       = length(invalid_data_raisings),
                row_indexes  = spreadsheet_rows_for(form, invalid_data_raisings),
                codes        = strata[invalid_data_raisings]$DATA_RAISING_CODE,
                codes_unique = unique(strata[invalid_data_raisings]$DATA_RAISING_CODE)
              ),
              missing = list(
                number      = length(missing_data_raisings),
                row_indexes = spreadsheet_rows_for(form, missing_data_raisings)
              )
            )
          ),
          coverage = list(
            type = list(
              invalid = list(
                number       = length(invalid_coverage_types),
                row_indexes  = spreadsheet_rows_for(form, invalid_coverage_types),
                codes        = strata[invalid_coverage_types]$COVERAGE_TYPE_CODE,
                codes_unique = unique(strata[invalid_coverage_types]$COVERAGE_TYPE_CODE)
              ),
              missing = list(
                number      = length(missing_coverage_types),
                row_indexes = spreadsheet_rows_for(form, missing_coverage_types)
              )
            ),
            value = list(
              invalid = list(
                number      = length(invalid_coverages),
                row_indexes = spreadsheet_rows_for(form, invalid_coverages),
                values      = strata[invalid_coverages]$COVERAGE
              ),
              missing = list(
                number      = length(missing_coverages),
                row_indexes = spreadsheet_rows_for(form, missing_coverages)
              )
            )
          )
        )
      ),
      records = list(
        total = nrow(data_CE_SF),
        empty_rows = list(
          number      = length(data_empty_rows),
          row_indexes = spreadsheet_rows_for(form, data_empty_rows)
        ),
        empty_columns = list(
          number      = length(data_empty_columns),
          col_indexes = spreadsheet_cols_for(form, data_empty_columns)
        )
      )
    )
  )
})

setMethod("common_data_validation_summary", list(form = "IOTCFormCESF", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  start = Sys.time()
  l_info("IOTCFormCESF.common_data_validation_summary")

  validation_messages = new("MessageList")

  ### STRATA AND RECORDS

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  validation_messages = report_strata(validation_messages, strata)

  # Strata checks

  ## Main strata

  checks_strata_main = checks_strata$main

  ### Months

  months = checks_strata_main$months

  # REMOVED, see also the comment in the validate_data above...
  #if(months$incomplete$number > 0)
  #  validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Data is not provided for all months within the strata in row(s) #", paste0(months$incomplete$row_indexes, collapse = ", "))))

  if(months$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing month in row(s) #", paste0(months$missing$row_indexes, collapse = ", "))))

  if(months$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid month value in row(s) #", paste0(months$invalid$row_indexes, collapse = ", "), ". Please use only 1-12 for Jan-Dec")))

  ### Fisheries

  validation_messages = report_fisheries(validation_messages, checks_strata_main$fisheries, "C")

  ### Grids

  grids = checks_strata_main$grids

  if(grids$missing$number > 0) { # This remains identical for 3-CE and 4-SF, whose 'GRID' column is the same
    if(grids$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = "C", text = paste0(grids$missing$number, " missing grids")))

    for(row in grids$missing$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = "C", text = paste0("Missing grid in row #", row)))
  }

  if(grids$invalid$number > 0) # TO BE CHECKED BETTER
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid in row(s) #", paste0(grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid grid codes")))

  ### Estimations

  estimations = checks_strata_main$estimations # NOT PART OF THE STRATUM
  estimation_col = estimation_column(form)

  if(estimations$missing$number > 0) { # Applies to 3CE and 4SF
    if(estimations$missing$number > 1) validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", column = estimation_col, text = paste0(estimations$missing$number, " missing estimation codes")))

    for(row in estimations$missing$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = estimation_col, text = paste0("Missing estimation code in row #", row)))
  }

  if(estimations$invalid$number > 0) { # Applies to 3CE and 4SF
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0(estimations$invalid$number, " invalid estimation code(s) reported. Please refer to ", reference_codes("data", "estimates"), " for a list of valid estimation codes")))

    for(row in estimations$invalid$row_indexes)
      validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", row = row, column = estimation_col, text = paste0("Invalid estimation code in row #", row)))
  }

  ## Original data

  checks_strata_original_data = checks_strata$original_data

  # DATA TYPES (1 per row, although not part of the stratum)

  validation_messages = report_data_type(validation_messages, checks_strata_original_data$type, "G")

  # DATA SOURCES (1 per row *and* part of the stratum)

  validation_messages = report_data_source(validation_messages, checks_strata_original_data$source, "H")

  # DATA PROCESSINGS (1 per row *and* part of the stratum)

  validation_messages = report_data_source(validation_messages, checks_strata_original_data$processing, "I")

  ### Data raisings

  raisings = checks_strata_original_data$raising # NOT PART OF THE STRATUM

  if(raisings$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing data raising in row(s) #", paste0(raisings$missing$row_indexes, collapse = ", "))))

  if(raisings$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid data raising in row(s) #", paste0(raisings$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "raisings"), " for a list of valid data raising codes")))

  # COVERAGE TYPES (1 per row, although not part of the stratum)

  validation_messages = report_coverage_type(validation_messages, checks_strata$coverage$type, "J")

  # COVERAGE VALUES (1 per row, although not part of the stratum)

  validation_messages = report_coverage_value(validation_messages, checks_strata$coverage$type, "J")

  # Data issues / summary

  ## Empty rows / columns

  validation_messages =

  return(
    report_data(
      validation_messages,
      records,
      allow_empty_data(form)
    )
  )
})
