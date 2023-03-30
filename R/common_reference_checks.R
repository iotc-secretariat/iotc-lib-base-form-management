### COMMON REFERENCE DATA CHECKS

validate_fleet = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  valid_fleets = unique(valid_fleets[, .(FLAG_CODE, REPORTING_ENTITY_CODE, FLEET_CODE, NAME_EN, NAME_FR)])

  reporting_entity_code = trim(reporting_entity_code)
  flag_country_code     = trim(flag_country_code)

  fleet = valid_fleets[REPORTING_ENTITY_CODE == reporting_entity_code &
                         FLAG_CODE == flag_country_code]

  if(nrow(fleet) == 0) stop(paste0("Unable to identify any valid fleet for reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))
  if(nrow(fleet) >  1) stop(paste0("Multiple fleets identified by reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))

  return(fleet)
}

validate_fishery = function(code) {
  code = trim(code)

  item = IN_FISHERIES[CODE == code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid fishery by code '", code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple fisheries identified by code '", code, "'")) # This should never happen...

  return(item)
}

validate_species = function(code) {
  code = trim(code)

  item = IN_SPECIES[CODE == code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid species by code '", code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple species identified by code '", code, "'")) # This should never happen...

  return(item)
}

validate_data_type = function(code) {
  code = trim(code)

  item = IN_DATA_TYPES[CODE == code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data type by code '", code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data types identified by code '", code, "'")) # This should never happen...

  return(item)
}

validate_data_source = function(dataset_code, data_source_code) {
  dataset_code     = trim(dataset_code)
  data_source_code = trim(data_source_code)

  item = IN_DATA_SOURCES[DATASET_CODE == dataset_code & CODE == data_source_code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data source by dataset '", dataset_code, "' and code '", data_source_code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data sources identified by dataset '", dataset_code, "' and code '", data_source_code, "'")) # This should never happen...

  return(item)
}

validate_data_processing = function(dataset_code, data_processing_code) {
  dataset_code     = trim(dataset_code)
  data_processing_code = trim(data_processing_code)

  item = IN_DATA_PROCESSINGS[DATASET_CODE == dataset_code & CODE == data_processing_code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data processing by dataset '", dataset_code, "' and code '", data_processing_code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data processings identified by dataset '", dataset_code, "' and code '", data_processing_code, "'")) # This should never happen...

  return(item)
}

validate_data_coverage_type = function(code) {
  code = trim(code)

  item = IN_DATA_COVERAGE_TYPES[CODE == code]

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data coverage type by code '", code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data coverage types identified by code '", code, "'")) # This should never happen...

  return(item)
}

is_multiple_gear_fishery = function(code) {
  return(
    grepl("\\+", validate_fishery(code)$CODE)
  )
}

is_species_aggregate = function(code) {
  return(
    validate_species(code)$IS_AGGREGATE
  )
}
