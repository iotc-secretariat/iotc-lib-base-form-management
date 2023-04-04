### COMMON REFERENCE DATA CHECKS

entities_for = function(entity_code) {
  return(
    IN_ENTITIES[CODE == trim(entity_code)]
  )
}

is_entity_valid = function(entity_code) {
  return(
    nrow(
      entities_for(
        entity_code
      )
    ) == 1
  )
}

validate_entity = function(entity_code, field = "Reporting entity") {
  entity_code = check_mandatory(trim(entity_code), field)

  entities = entities_for(entity_code)

  if(nrow(entities) == 0) stop(paste0("Unable to identify any valid entity by code '", entity_code, "'"))
  if(nrow(entities) >  1) stop(paste0("Multiple entities identified by code '", entity_code, "'")) # This should never happen...

  return(entities)
}


countries_for = function(country_code) {
  return(
    IN_COUNTRIES[CODE == trim(country_code)]
  )
}

is_country_valid = function(country_code) {
  return(
    nrow(
      countries_for(
        country_code
      )
    ) == 1
  )
}

validate_country = function(country_code, field = "Flag country") {
  country_code = check_mandatory(trim(country_code), field)

  countries = entities_for(country_code)

  if(nrow(countries) == 0) stop(paste0("Unable to identify any valid country by code '", country_code, "'"))
  if(nrow(countries) >  1) stop(paste0("Multiple countries identified by code '", country_code, "'")) # This should never happen...

  return(countries)
}

fleets_for = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  valid_fleets = unique(valid_fleets[, .(FLAG_CODE, REPORTING_ENTITY_CODE, FLEET_CODE, NAME_EN, NAME_FR)])

  reporting_entity_code = trim(reporting_entity_code)
  flag_country_code     = trim(flag_country_code)

  fleets = valid_fleets[REPORTING_ENTITY_CODE == reporting_entity_code &
                        FLAG_CODE             == flag_country_code]

  return(fleets)
}

is_fleet_valid = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  return(nrow(fleet) == 1)
}

validate_fleet = function(reporting_entity_code, flag_country_code, valid_fleets = IN_FLEETS_FLAGS) {
  reporting_entity_code = validate_entity(reporting_entity_code)$CODE
  flag_country_code     = validate_country(flag_country_code)$CODE

  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  if(nrow(fleets) == 0) stop(paste0("Unable to identify any valid fleet for reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))
  if(nrow(fleets) >  1) stop(paste0("Multiple fleets identified by reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))

  return(fleets)
}

fisheries_for = function(fishery_code) {
  return(
    IN_FISHERIES[CODE == trim(fishery_code)]
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

validate_fishery = function(fishery_code, field = "Fishery") {
  fishery_code = check_mandatory(trim(fishery_code), field)

  fisheries = fisheries_for(fishery_code)

  if(nrow(fisheries) == 0) stop(paste0("Unable to identify any valid fishery by code '", fishery_code, "'"))
  if(nrow(fisheries) >  1) stop(paste0("Multiple fisheries identified by code '", fishery_code, "'")) # This should never happen...

  return(fisheries)
}

is_multiple_gear_fishery = function(fishery_code) {
  return(
    grepl("\\+", fisheries_for(fishery_code)$CODE)
    #grepl("\\+", fishery_code)
  )
}

IOTC_main_areas_for = function(IOTC_main_area_code) {
  return(
    IN_IOTC_AREA[CODE == trim(IOTC_main_area_code)]
  )
}

is_IOTC_main_area_valid = function(IOTC_main_area_code) {
  return(
    nrow(
      IOTC_main_areas_for(
        IOTC_main_area_code
      )
    ) == 1
  )
}

validate_IOTC_main_area = function(IOTC_main_area_code, field = "IOTC main area") {
  IOTC_main_area_code = check_mandatory(trim(IOTC_main_area_code), field)

  IOTC_main_areas = IOTC_main_areas_for(IOTC_main_area_code)

  if(nrow(IOTC_main_areas) == 0) stop(paste0("Unable to identify any valid IOTC main area by code '", IOTC_main_area_code, "'"))
  if(nrow(IOTC_main_areas) >  1) stop(paste0("Multiple IOTC main areas identified by code '", IOTC_main_area_code, "'")) # This should never happen...

  return(IOTC_main_areas)
}

species_for = function(species_code) {
  return(
    IN_SPECIES[CODE == trim(species_code)]
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

validate_species = function(species_code, field = "Species") {
  species_code = check_mandatory(trim(species_code), field)

  species = species_for(species_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid species by code '", species_code, "'"))
  if(nrow(species) >  1) stop(paste0("Multiple species identified by code '", species_code, "'")) # This should never happen...

  return(species)
}

is_species_aggregate = function(species_code) {
  return(
    species_for(species_code)$IS_AGGREGATE == TRUE
  )
}

data_type_for = function(data_type_code) {
  return(
    IN_DATA_TYPES[CODE == trim(data_type_code)]
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

validate_data_type = function(data_type_code, field = "Data type") {
  data_type_code = check_mandatory(trim(data_type_code), field)

  data_types = data_types_for(data_type_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid data type by code '", data_type_code, "'"))
  if(nrow(species) >  1) stop(paste0("Multiple data types identified by code '", data_type_code, "'")) # This should never happen...

  return(species)
}

data_source_for = function(dataset_code, data_source_code) {
  dataset_code = trim(dataset_code)
  data_source_code = trim(data_source_code)

  return(
    IN_DATA_SOURCES[DATASET_CODE == dataset_code &
                    CODE == data_source_code]
  )
}

is_data_source_valid = function(dataset_code, data_source_code) {
  return(
    nrow(
      data_source_for(
        dataset_code,
        data_source_code
      )
    ) == 1
  )
}

validate_data_source = function(dataset_code, data_source_code, field = "Data source") {
  dataset_code     = check_mandatory(trim(dataset_code), "Dataset")
  data_source_code = check_mandatory(trim(data_source_code), field)

  data_sources = data_source_for(dataset_code, data_source_code)

  if(nrow(data_sources) == 0) stop(paste0("Unable to identify any valid data source by dataset '", dataset_code, "' and code '", data_source_code, "'"))
  if(nrow(data_sources) >  1) stop(paste0("Multiple data sources identified by dataset '", dataset_code, "' and code '", data_source_code, "'")) # This should never happen...

  return(data_sources)
}

data_processing_for = function(dataset_code, data_processing_code) {
  return(
    IN_DATA_PROCESSINGS[DATASET_CODE == trim(dataset_code) &
                        CODE == trim(data_processing_code)]
  )
}

is_data_processing_valid = function(dataset_code, data_processing_code) {
  return(
    nrow(
      data_processing_for(
        dataset_code,
        data_processing_code
      )
    ) == 1
  )
}

validate_data_processing = function(dataset_code, data_processing_code, field = "Data processing") {
  dataset_code         = check_mandatory(trim(dataset_code), "Dataset")
  data_processing_code = check_mandatory(trim(data_processing_code), field)

  data_processings = data_processing_for(dataset_code, data_processing_code)

  if(nrow(data_processings) == 0) stop(paste0("Unable to identify any valid data processing by dataset '", dataset_code, "' and code '", data_processing_code, "'"))
  if(nrow(data_processings) >  1) stop(paste0("Multiple data processings identified by dataset '", dataset_code, "' and code '", data_processing_code, "'")) # This should never happen...

  return(data_processings)
}

data_coverage_type_for = function(data_coverage_type_code) {
  return(
    IN_DATA_COVERAGE_TYPES[CODE == trim(data_coverage_type_code)]
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

validate_data_coverage_type = function(data_coverage_type_code, field = "Data coverage type") {
  data_coverage_type_code = check_mandatory(trim(data_coverage_type_code), field)

  data_coverages = data_coverage_type_for(data_coverage_type_code)

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data coverage type by code '", data_coverage_type_code, "'"))
  if(nrow(item) >  1) stop(paste0("Multiple data coverage types identified by code '", data_coverage_type_code, "'")) # This should never happen...

  return(data_coverages)
}

fate_for = function(type_of_fate_code, fate_code) {
  type_of_fate_code = trim(type_of_fate_code)
  fate_code         = trim(fate_code)

  return(
    IN_FATES[TYPE_OF_FATE_CODE == type_of_fate_code &
             CODE              == fate_code]
  )
}

is_fate_valid = function(type_of_fate_code, fate_code) {
  return(
    nrow(
      fate_for(
        type_of_fate_code,
        fate_code
      )
    ) == 1
  )
}

validate_fate = function(type_of_fate_code, fate_code, field = "Fate") {
  type_of_fate_code = check_mandatory(trim(type_of_fate_code), "Type of fate")
  fate_code         = check_mandatory(trim(fate_code), field)

  fates = fate_for(type_of_fate_code, fate_code)

  if(nrow(fates) == 0) stop(paste0("Unable to identify any valid fate by type of fate '", type_of_fate_code, "' and code '", fate_code, "'"))
  if(nrow(fates) >  1) stop(paste0("Multiple fates identified by type of fate '", type_of_fate_code, "' and code '", fate_code, "'")) # This should never happen...

  return(fates)
}

is_retain_reason_valid = function(retain_reason_code) {
  return(
    is_fate_valid(
      "RE", retain_reason_code
    )
  )
}

validate_retain_reason = function(retain_reason_code, field = "Retain reason") {
  return(
    validate_fate(
      "RE",
      retain_reason_code,
      field
    )
  )
}

is_discard_reason_valid = function(discard_reason_code) {
  return(
    is_fate_valid(
      "DI", discard_reason_code
    )
  )
}

validate_discard_reason = function(discard_reason_code, field = "Discard reason") {
  return(
    validate_fate(
      "DI",
      discard_reason_code,
      field
    )
  )
}
