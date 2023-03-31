### COMMON REFERENCE DATA CHECKS

fleets_for = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  reporting_entity_code = check_mandatory(trim(reporting_entity_code), "Reporting entity code")
  flag_country_code     = check_mandatory(trim(flag_country_code), "Flag country code")

  valid_fleets = unique(valid_fleets[, .(FLAG_CODE, REPORTING_ENTITY_CODE, FLEET_CODE, NAME_EN, NAME_FR)])

  fleets = valid_fleets[REPORTING_ENTITY_CODE == reporting_entity_code &
                        FLAG_CODE == flag_country_code]

  return(fleets)
}

is_fleet_valid = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  return(nrow(fleet) == 1)
}

validate_fleet = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  if(nrow(fleets) == 0) stop(paste0("Unable to identify any valid fleet for reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))
  if(nrow(fleets) >  1) stop(paste0("Multiple fleets identified by reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))

  return(fleets)
}

fisheries_for = function(fishery_code) {
  return(
    IN_FISHERIES[CODE == check_mandatory(trim(fishery_code))]
  )
}

is_fishery_valid = function(fishery_code) {
  return(
    nrow(
      fisheries_for(
        fishery_code
      )
    ) == 1
  )
}

validate_fishery = function(fishery_code) {
  fisheries = fisheries_for(fishery_code)

  if(nrow(fisheries) == 0) stop(paste0("Unable to identify any valid fishery by code '", fishery_code, "'"))
  if(nrow(fisheries) >  1) stop(paste0("Multiple fisheries identified by code '", fishery_code, "'")) # This should never happen...

  return(fisheries)
}

species_for = function(species_code) {
  return(
    IN_SPECIES[CODE == check_mandatory(trim(species_code), "Species code")]
  )
}

is_species_valid = function(species_code) {
  return(
    nrow(
      species_for(
        species_code
      )
    ) == 1
  )
}

validate_species = function(species_code) {
  species = species_for(species_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid species by code '", species_code, "'"))
  if(nrow(species) >  1) stop(paste0("Multiple species identified by code '", species_code, "'")) # This should never happen...

  return(species)
}

data_type_for = function(data_type_code) {
  return(
    IN_DATA_TYPES[CODE == check_mandatory(trim(data_type_code), "Data type code")]
  )
}

is_data_type_valid = function(data_type_code) {
  return(
    nrow(
      data_type_for(
        data_type_code
      )
    ) == 1
  )
}

validate_data_type = function(data_type_code) {
  data_types = data_types_for(data_type_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid data type by code '", data_type_code, "'"))
  if(nrow(species) >  1) stop(paste0("Multiple data types identified by code '", data_type_code, "'")) # This should never happen...

  return(species)
}

data_source_for = function(dataset_code, data_source_code) {
  return(
    IN_DATA_SOURCES[DATASET_CODE == check_mandatory(trim(dataset_code), "Dataset code") &
                    DATA_SOURCE_CODE == check_mandatory(trim(data_source_code), "Data source code")]
  )
}

is_data_source_valid = function(data_source_code) {
  return(
    nrow(
      data_source_for(
        data_source_code
      )
    ) == 1
  )
}

validate_data_source = function(dataset_code, data_source_code) {
  data_sources = data_source_for(dataset_code, data_source_code)

  if(nrow(data_sources) == 0) stop(paste0("Unable to identify any valid data source by dataset '", dataset_code, "' and code '", data_source_code, "'"))
  if(nrow(data_sources) >  1) stop(paste0("Multiple data sources identified by dataset '", dataset_code, "' and code '", data_source_code, "'")) # This should never happen...

  return(data_sources)
}

data_processing_for = function(dataset_code, data_processing_code) {
  return(
    IN_DATA_PROCESSINGS[DATASET_CODE == check_mandatory(trim(dataset_code), "Dataset code") &
                        DATA_PROCESSING_CODE == check_mandatory(trim(data_processing_code), "Data processing code")]
  )
}

is_data_processing_valid = function(data_processing_code) {
  return(
    nrow(
      data_processing_for(
        data_processing_code
      )
    ) == 1
  )
}

validate_data_processing = function(dataset_code, data_processing_code) {
  data_processings = data_processing_for(dataset_code, data_processing_code)

  if(nrow(data_processings) == 0) stop(paste0("Unable to identify any valid data processing by dataset '", dataset_code, "' and code '", data_processing_code, "'"))
  if(nrow(data_processings) >  1) stop(paste0("Multiple data processings identified by dataset '", dataset_code, "' and code '", data_processing_code, "'")) # This should never happen...

  return(data_processings)
}


data_coverage_type_for = function(data_coverage_type_code) {
  return(
    IN_DATA_COVERAGE_TYPES[CODE == check_mandatory(trim(data_coverage_type_code), "Data coverage type code")]
  )
}

is_data_coverage_type_valid = function(data_coverage_type_code) {
  return(
    nrow(
      data_coverage_type_for(
        data_coverage_type_code
      )
    ) == 1
  )
}

validate_data_coverage_type = function(data_coverage_type_code) {
  data_coverages = data_coverage_type_for(data_coverage_type_code)

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data coverage type by code '", data_coverage_type_code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data coverage types identified by code '", data_coverage_type_code, "'")) # This should never happen...

  return(data_coverages)
}

is_multiple_gear_fishery = function(fishery_code) {
  return(
    grepl("\\+", validate_fishery(fishery_code)$CODE)
  )
}

is_species_aggregate = function(species_code) {
  return(
    validate_species(species_code)$IS_AGGREGATE
  )
}
