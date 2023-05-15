#' @include IOTC_form_class.R
#' @export IOTCFormCESFMultiple
IOTCFormCESFMultiple = setClass(
  "IOTCFormCESFMultiple",
  contains = "IOTCForm"
)

setGeneric("grid_validator", function(form) {
  standardGeneric("grid_validator")
})

setGeneric("allow_empty_data", function(form) {
  standardGeneric("allow_empty_data")
})

setGeneric("optional_strata_columns", function(form) {
  standardGeneric("optional_strata_columns")
})

setGeneric("validate_months", function(form, strata) {
  standardGeneric("validate_months")
})

setMethod("form_comment_cell_row", "IOTCFormCESFMultiple", function(form) {
  return(23) #Default for CE / SF (multiple)
})

setMethod("extract_metadata", list(form = "IOTCFormCESFMultiple", common_metadata = "list"), function(form, common_metadata) {
  l_info("IOTCFormCESFMultiple.extract_metadata")

  return(common_metadata)
})

setMethod("validate_metadata", list(form = "IOTCFormCESFMultiple", common_metadata_validation_results = "list"), function(form, common_metadata_validation_results) {
  l_info("IOTCFormCESFMultiple.validate_metadata")

  return(common_metadata_validation_results)
})

setMethod("metadata_validation_summary", list(form = "IOTCFormCESFMultiple", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESFMultiple.metadata_validation_summary")

  return(new("MessageList"))
})

setMethod("validate_data", list(form = "IOTCFormCESFMultiple", metadata_validation_results = "list"), function(form, metadata_validation_results) {
  l_info("IOTCFormCESFMultiple.validate_data")

  strata  = form@data$strata
  records = form@data$records

  data_CE_SF_original = records$data$CE_SF_data_original
  data_CE_SF          = records$data$CE_SF_data

  start = Sys.time()

  strata_empty_rows    = find_empty_rows(strata)
  strata_empty_columns = find_empty_columns(strata)
  strata_empty_columns = strata_empty_columns[which(!strata_empty_columns %in% optional_strata_columns(form))]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (I): ", Sys.time() - start))
  start = Sys.time()

  strata[, IS_EMPTY := .I %in% strata_empty_rows]

  total_strata     = nrow(strata)
  non_empty_strata = which(strata$IS_EMPTY == FALSE) #strata[ !1:.N %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (II): ", Sys.time() - start))
  start = Sys.time()

  data_empty_rows    = find_empty_rows(data_CE_SF)
  data_empty_columns = find_empty_columns(data_CE_SF)

  l_info(paste0("IOTCFormCESFMultiple.validate_data (III): ", Sys.time() - start))
  start = Sys.time()

  missing_months   = which( sapply(strata$MONTH, is.na))
  invalid_months   = which(!sapply(strata$MONTH, is_month_valid))

  l_info(paste0("IOTCFormCESFMultiple.validate_data (IV): ", Sys.time() - start))
  start = Sys.time()

  invalid_months   = invalid_months[ ! invalid_months %in% missing_months ]
  missing_months   = missing_months[ ! missing_months %in% strata_empty_rows]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (V): ", Sys.time() - start))
  start = Sys.time()

  # If all months are provided and valid, we check that they're also consistent...

  months_check = validate_months(form, strata)

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.a): ", Sys.time() - start))
  start = Sys.time()

  missing_fisheries  = which( sapply(strata$FISHERY_CODE, is.na))
  invalid_fisheries  = which(!sapply(strata$FISHERY_CODE, is_fishery_valid))
  invalid_fisheries  = invalid_fisheries[ ! invalid_fisheries %in% missing_fisheries ]
  missing_fisheries  = missing_fisheries[ ! missing_fisheries %in% strata_empty_rows]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.b): ", Sys.time() - start))
  start = Sys.time()

  find_fishery_type = function(code) {
    if(!is.na(code) && is_fishery_valid(code)) {
      return(fisheries_for(code)$FISHERY_TYPE_CODE)
    }

    return(NA)
  }

  fishery_types      = sapply(strata$FISHERY_CODE, find_fishery_type)
  fishery_aggregates =
    which(
      unlist(
        sapply(
          strata$FISHERY_CODE,
          function(value) {
            return(!is.na(value) && is_multiple_gear_fishery(value))
          },
          USE.NAMES = FALSE
        ),
        use.names = FALSE
      )
    )

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.c): ", Sys.time() - start))
  start = Sys.time()

  missing_target_species = which( sapply(strata$TARGET_SPECIES_CODE, is.na))
  invalid_target_species = which(!sapply(strata$TARGET_SPECIES_CODE, is_species_valid))
  invalid_target_species = invalid_target_species[ ! invalid_target_species %in% missing_target_species ]
  missing_target_species = missing_target_species[ ! missing_target_species %in% strata_empty_rows]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.d): ", Sys.time() - start))
  start = Sys.time()

  missing_grids = which( sapply(strata$GRID_CODE, is.na))
  invalid_grids = which(!sapply(strata$GRID_CODE, grid_validator(form)))
  invalid_grids = invalid_grids[ ! invalid_grids %in% missing_grids ]
  missing_grids = missing_grids[ ! missing_grids %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.e): ", Sys.time() - start))
  start = Sys.time()

  valid_grids   = strata$GRID_CODE
  valid_grids   = which(!sapply(strata$GRID_CODE, is.na))

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.f): ", Sys.time() - start))
  start = Sys.time()

  missing_estimations    = which( sapply(strata$ESTIMATION_CODE, is.na))
  invalid_estimations    = which(!sapply(strata$ESTIMATION_CODE, is_data_estimation_valid))
  invalid_estimations    = invalid_estimations[ ! invalid_estimations %in% missing_estimations ]
  missing_estimations    = missing_estimations[ ! missing_estimations %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.g): ", Sys.time() - start))
  start = Sys.time()

  missing_types_of_data    = which( sapply(strata$DATA_TYPE_CODE, is.na))
  invalid_types_of_data    = which(!sapply(strata$DATA_TYPE_CODE, is_data_type_valid))
  invalid_types_of_data    = invalid_types_of_data[ ! invalid_types_of_data %in% missing_types_of_data ]
  missing_types_of_data    = missing_types_of_data[ ! missing_types_of_data %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.h): ", Sys.time() - start))
  start = Sys.time()

  missing_data_sources     = which( sapply(strata$DATA_SOURCE_CODE, is.na))
  invalid_data_sources     = which(!sapply(strata$DATA_SOURCE_CODE, function(code) { return(is_data_source_valid(form_dataset_code(form), code)) }))
  invalid_data_sources     = invalid_data_sources[ ! invalid_data_sources %in% missing_data_sources ]
  missing_data_sources     = missing_data_sources[ ! missing_data_sources %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.i): ", Sys.time() - start))
  start = Sys.time()

  missing_data_processings = which( sapply(strata$DATA_PROCESSING_CODE, is.na))
  invalid_data_processings = which(!sapply(strata$DATA_PROCESSING_CODE, function(code) { return(is_data_processing_valid(form_dataset_code(form), code)) }))
  invalid_data_processings = invalid_data_processings[ ! invalid_data_processings %in% missing_data_processings ]
  missing_data_processings = missing_data_processings[ ! missing_data_processings %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.j): ", Sys.time() - start))
  start = Sys.time()

  missing_data_raisings    = which( sapply(strata$DATA_RAISING_CODE, is.na))
  invalid_data_raisings    = which(!sapply(strata$DATA_RAISING_CODE, is_data_raising_valid))
  invalid_data_raisings    = invalid_data_raisings[ ! invalid_data_raisings %in% missing_data_raisings ]
  missing_data_raisings    = missing_data_raisings[ ! missing_data_raisings %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.k): ", Sys.time() - start))
  start = Sys.time()

  missing_coverage_types   = which( sapply(strata$COVERAGE_TYPE_CODE, is.na))
  invalid_coverage_types   = which(!sapply(strata$COVERAGE_TYPE_CODE, is_data_coverage_type_valid))
  invalid_coverage_types   = invalid_coverage_types[ ! invalid_coverage_types %in% missing_coverage_types ]
  missing_coverage_types   = missing_coverage_types[ ! missing_coverage_types %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.l): ", Sys.time() - start))
  start = Sys.time()

  # Temporarily added
  strata$COVERAGE = as.double(strata$COVERAGE)

  missing_coverages        = which( sapply(strata$COVERAGE, is.na))
  invalid_coverages        = which(!sapply(strata$COVERAGE, is_percentage_valid))
  invalid_coverages        = invalid_coverages[ ! invalid_coverages %in% missing_coverages ]
  missing_coverages        = missing_coverages[ ! missing_coverages %in% strata_empty_rows ]

  l_info(paste0("IOTCFormCESFMultiple.validate_data (VI.m): ", Sys.time() - start))
  start = Sys.time()

  return(
    list(
      strata = list(
        empty_rows = list(
          number      = length(strata_empty_rows),
          row_indexes = strata_empty_rows
        ),
        empty_columns = list(
          number      = length(strata_empty_columns),
          col_indexes = strata_empty_columns
        ),
        total = list(
          number = total_strata
        ),
        non_empty = list(
          number = length(non_empty_strata),
          row_indexes = non_empty_strata
        ),
        checks = list(
          main = list(
            months = list(
              missing = list(
                number      = length(missing_months),
                row_indexes = missing_months
              ),
              invalid = list(
                number        = length(invalid_months),
                row_indexes   = invalid_months,
                values        = strata$MONTH[invalid_months],
                values_unique = unique(strata$MONTH[invalid_months])
              )
              # REMOVED: while we expect data to be provided for all quarters in 1-RC and 1-DI, same is not the case for 3-CE or 4-SF where stratification is much finer
              #, incomplete = list(
              #  number      = length(months_check$incomplete_months),
              #  row_indexes = months_check$incomplete_months
              #)
            ),
            fisheries = list(
              invalid = list(
                number       = length(invalid_fisheries),
                row_indexes  = invalid_fisheries,
                codes        = strata$FISHERY_CODE[invalid_fisheries],
                codes_unique = unique(strata$FISHERY_CODE[invalid_fisheries])
              ),
              missing = list(
                number      = length(missing_fisheries),
                row_indexes = missing_fisheries
              ),
              aggregates = list(
                number       = length(fishery_aggregates),
                row_indexes  = fishery_aggregates,
                codes        = strata[fishery_aggregates]$FISHERY_CODE,
                codes_unique = unique(strata[fishery_aggregates]$FISHERY_CODE)
              ),
              types = list(
                artisanal = list(
                  number      = length(which(fishery_types == "AR")),
                  row_indexes = which(fishery_types == "AR")
                ),
                semi_industrial = list(
                  number      = length(which(fishery_types == "SI")),
                  row_indexes = which(fishery_types == "SI")
                ),
                industrial = list(
                  number      = length(which(fishery_types == "IN")),
                  row_indexes = which(fishery_types == "IN")
                )
              )
            ),
            target_species = list(
              invalid = list(
                number       = length(invalid_target_species),
                row_indexes  = invalid_target_species,
                codes        = strata$TARGET_SPECIES_CODE[invalid_target_species],
                codes_unique = unique(strata$TARGET_SPECIES_CODE[invalid_target_species])
              ),
              missing = list(
                number      = length(missing_target_species),
                row_indexes = missing_target_species
              )
            ),
            grids = list(
              invalid = list(
                number       = length(invalid_grids),
                row_indexes  = invalid_grids,
                codes        = strata[invalid_grids]$GRID_CODE,
                codes_unique = unique(strata[invalid_grids]$GRID_CODE)
              ),
              missing = list(
                number      = length(missing_grids),
                row_indexes = missing_grids
              )
            ),
            estimations = list(
              invalid = list(
                number       = length(invalid_estimations),
                row_indexes  = invalid_estimations,
                codes        = strata[invalid_estimations]$ESTIMATION_CODE,
                codes_unique = unique(strata[invalid_estimations]$ESTIMATION_CODE)
              ),
              missing = list(
                number      = length(missing_estimations),
                row_indexes = missing_estimations
              )
            )
          ),
          original_data = list(
            type = list(
              invalid = list(
                number       = length(invalid_types_of_data),
                row_indexes  = invalid_types_of_data,
                codes        = strata[invalid_types_of_data]$DATA_TYPE_CODE,
                codes_unique = unique(strata[invalid_types_of_data]$DATA_TYPE_CODE)
              ),
              missing = list(
                number      = length(missing_types_of_data),
                row_indexes = missing_types_of_data
              )
            ),
            source = list(
              invalid = list(
                number       = length(invalid_data_sources),
                row_indexes  = invalid_data_sources,
                codes        = strata[invalid_data_sources]$DATA_SOURCE_CODE,
                codes_unique = unique(strata[invalid_data_sources]$DATA_SOURCE_CODE)
              ),
              missing = list(
                number      = length(missing_data_sources),
                row_indexes = missing_data_sources
              )
            ),
            processing = list(
              invalid = list(
                number       = length(invalid_data_processings),
                row_indexes  = invalid_data_processings,
                codes        = strata[invalid_data_processings]$DATA_PROCESSING_CODE,
                codes_unique = unique(strata[invalid_data_processings]$DATA_PROCESSING_CODE)
              ),
              missing = list(
                number      = length(missing_data_processings),
                row_indexes = missing_data_processings
              )
            ),
            raising = list(
              invalid = list(
                number       = length(invalid_data_raisings),
                row_indexes  = invalid_data_raisings,
                codes        = strata[invalid_data_raisings]$DATA_RAISING_CODE,
                codes_unique = unique(strata[invalid_data_raisings]$DATA_RAISING_CODE)
              ),
              missing = list(
                number      = length(missing_data_raisings),
                row_indexes = missing_data_raisings
              )
            )
          ),
          coverage = list(
            type = list(
              invalid = list(
                number       = length(invalid_coverage_types),
                row_indexes  = invalid_coverage_types,
                codes        = strata[invalid_coverage_types]$COVERAGE_TYPE_CODE,
                codes_unique = unique(strata[invalid_coverage_types]$COVERAGE_TYPE_CODE)
              ),
              missing = list(
                number      = length(missing_coverage_types),
                row_indexes = missing_coverage_types
              )
            ),
            value = list(
              invalid = list(
                number      = length(invalid_coverages),
                row_indexes = invalid_coverages,
                values      = strata[invalid_coverages]$COVERAGE
              ),
              missing = list(
                number      = length(missing_coverages),
                row_indexes = missing_coverages
              )
            )
          )
        )
      ),
      records = list(
        total = nrow(data_CE_SF),
        empty_rows = list(
          number      = length(data_empty_rows),
          row_indexes = data_empty_rows
        ),
        empty_columns = list(
          number      = length(data_empty_columns),
          col_indexes = data_empty_columns
        )
      )
    )
  )
})

setMethod("common_data_validation_summary", list(form = "IOTCFormCESFMultiple", metadata_validation_results = "list", data_validation_results = "list"), function(form, metadata_validation_results, data_validation_results) {
  l_info("IOTCFormCESFMultiple.common_data_validation_summary")

  validation_messages = new("MessageList")

  ### STRATA AND RECORDS

  strata  = data_validation_results$strata
  records = data_validation_results$records

  checks_strata  = strata$checks
  checks_records = records$checks

  # Strata issues / summary

  validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$total$number,     " total strata")))
  validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$non_empty$number, " non-empty strata")))
  validation_messages = add(validation_messages, new("Message", level = "INFO", source = "Data", text = paste0(strata$unique$number,    " unique strata")))

  if(strata$empty_rows$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_rows$number,    " empty strata detected: see row(s) #", paste0(strata$empty_rows$row_indexes, collapse = ", "))))

  if(strata$empty_columns$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$empty_columns$number, " empty strata columns detected: see column(s) #", paste0(strata$empty_columns$col_indexes, collapse = ", "))))

  if(strata$duplicate$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(strata$duplicate$number,     " duplicate strata detected: see row(s) #", paste0(strata$duplicate$row_indexes, collapse = ", "))))

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

  fisheries = checks_strata_main$fisheries

  if(fisheries$aggregates$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "WARN", source = "Data", text = paste0("Aggregated fisheries in row(s) #", paste0(fisheries$aggregates$row_indexes, collapse = ", "))))

  if(fisheries$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing fishery in row(s) #", paste0(fisheries$missing$row_indexes, collapse = ", "))))

  if(fisheries$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid fishery in row(s) #", paste0(fisheries$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid legacy fishery codes")))

  ### Target species

  target_species = checks_strata_main$target_species

  if(target_species$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing target species in row(s) #", paste0(target_species$missing$row_indexes, collapse = ", "))))

  if(target_species$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid target species in row(s) #", paste0(target_species$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("legacy", "species"), " for a list of valid legacy species codes")))

  ### Grids

  grids = checks_strata_main$grids

  if(grids$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing grid in row(s) #", paste0(grids$missing$row_indexes, collapse = ", "))))

  if(grids$invalid$number > 0) # TO BE CHECKED BETTER
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid grid in row(s) #", paste0(grids$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid grid codes")))

  ### Estimations

  estimations = checks_strata_main$estimations

  if(estimations$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing estimation code in row(s) #", paste0(estimations$missing$row_indexes, collapse = ", "))))

  if(estimations$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid estimation code in row(s) #", paste0(estimations$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "estimates"), " for a list of valid estimation codes")))

  ## Original data

  checks_strata_original_data = checks_strata$original_data

  ### Types

  types = checks_strata_original_data$type # NOT PART OF THE STRATUM

  if(types$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data type in row(s) #", paste0(types$missing$row_indexes, collapse = ", "))))

  if(types$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data type in row(s) #", paste0(types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

  ### Data sources

  sources = checks_strata_original_data$source

  if(sources$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data source in row(s) #", paste0(sources$missing$row_indexes, collapse = ", "))))

  if(sources$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data source in row(s) #", paste0(sources$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes for this dataset")))

  ### Data processings

  processings = checks_strata_original_data$processing

  if(processings$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing original data processing in row(s) #", paste0(processings$missing$row_indexes, collapse = ", "))))

  if(processings$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid original data processing in row(s) #", paste0(processings$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes for this dataset")))

  ### Data raisings

  raisings = checks_strata_original_data$raising # NOT PART OF THE STRATUM

  if(raisings$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing data raising in row(s) #", paste0(raisings$missing$row_indexes, collapse = ", "))))

  if(raisings$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid data raising in row(s) #", paste0(raisings$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "raisings"), " for a list of valid data raising codes")))

  ### Coverage types

  checks_strata_coverage = checks_strata$coverage # NOT PART OF THE STRATUM

  coverage_types = checks_strata_coverage$type

  if(coverage_types$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing coverage type in row(s) #", paste0(coverage_types$missing$row_indexes, collapse = ", "))))

  if(coverage_types$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid coverage type in row(s) #", paste0(coverage_types$invalid$row_indexes, collapse = ", "), ". Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid coverage type codes")))

  ### Coverage values

  coverage_values = checks_strata_coverage$value # NOT PART OF THE STRATUM

  if(coverage_values$missing$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Missing coverage value in row(s) #", paste0(coverage_values$missing$row_indexes, collapse = ", "))))

  if(coverage_values$invalid$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "ERROR", source = "Data", text = paste0("Invalid coverage value in row(s) #", paste0(coverage_values$invalid$row_indexes, collapse = ", "))))

  # Data issues / summary

  ## Empty rows / columns
  empty_rows = records$empty_rows

  if(empty_rows$number > 0 && !allow_empty_data(form))
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_rows$number, " empty data records detected: see row(s) #", paste0(empty_rows$row_indexes, collapse = ", "))))

  empty_columns = records$empty_columns

  if(empty_columns$number > 0)
    validation_messages = add(validation_messages, new("Message", level = "FATAL", source = "Data", text = paste0(empty_columns$number, " empty data columns detected: see column(s) #", paste0(empty_columns$col_indexes, collapse = ", "))))

  return(validation_messages)
})
