### COMMON REFERENCE DATA CHECKS

## ENTITIES

entities_for = function(entity_code) {
  return(
    iotc.data.reference.codelists::ENTITIES[CODE %in% trim(entity_code)]
  )
}

is_entity_valid = function(entity_code) {
  return(
    trim(entity_code) %in% iotc.data.reference.codelists::ENTITIES$CODE
  )
}

validate_entity = function(entity_code, field = "Reporting entity") {
  entity_code = check_mandatory(trim(entity_code), field)

  entities = entities_for(entity_code)

  if(nrow(entities) == 0) stop(paste0("Unable to identify any valid entity by code '", entity_code, "'. Please refer to ", reference_codes("admin", "entities"), " for a list of valid reporting entity codes"))
  if(nrow(entities) >  1) stop(paste0("Multiple entities identified by code '", entity_code, "'")) # This should never happen...

  return(entities)
}

## COUNTRIES

countries_for = function(country_code) {
  return(
    iotc.data.reference.codelists::COUNTRIES[CODE %in% trim(country_code)]
  )
}

is_country_valid = function(country_code) {
  return(
    trim(country_code) %in% iotc.data.reference.codelists::COUNTRIES$CODE
  )
}

validate_country = function(country_code, field = "Flag country") {
  country_code = check_mandatory(trim(country_code), field)

  countries = entities_for(country_code)

  if(nrow(countries) == 0) stop(paste0("Unable to identify any valid country by code '", country_code, "'. Please refer to ", reference_codes("admin", "countries"), " for a list of valid flag country codes"))
  if(nrow(countries) >  1) stop(paste0("Multiple countries identified by code '", country_code, "'")) # This should never happen...

  return(countries)
}

## FLEETS

fleets_for = function(reporting_entity_code, flag_country_code, valid_fleets = iotc.data.reference.codelists::LEGACY_FLEETS) {
  valid_fleets = unique(valid_fleets[, .(FLAG_CODE, REPORTING_ENTITY_CODE, FLEET_CODE, NAME_EN, NAME_FR)])

  reporting_entity_code = trim(reporting_entity_code)
  flag_country_code     = trim(flag_country_code)

  fleets = valid_fleets[REPORTING_ENTITY_CODE == reporting_entity_code &
                        FLAG_CODE             == flag_country_code]

  return(fleets)
}

is_fleet_valid = function(reporting_entity_code, flag_country_code, valid_fleets = iotc.data.reference.codelists::LEGACY_FLEETS) {
  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  return(nrow(fleets) == 1 && is_available(fleets))
}

validate_fleet = function(reporting_entity_code, flag_country_code, valid_fleets = iotc.data.reference.codelists::LEGACY_FLEETS) {
  reporting_entity_code = validate_entity(reporting_entity_code)$CODE
  flag_country_code     = validate_country(flag_country_code)$CODE

  fleets = fleets_for(reporting_entity_code, flag_country_code, valid_fleets)

  if(nrow(fleets) == 0) stop(paste0("Unable to identify any valid fleet for reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))
  if(nrow(fleets) >  1) stop(paste0("Multiple fleets identified by reporting entity '", reporting_entity_code, "' and flag '", flag_country_code, "'"))

  return(fleets)
}

## FISHERIES

fisheries_for = function(fishery_code) {
  return(
    iotc.data.reference.codelists::LEGACY_FISHERIES[CODE %in% trim(fishery_code)]
  )
}

is_fishery_valid = function(fishery_code) {
  return(
    trim(fishery_code) %in% iotc.data.reference.codelists::LEGACY_FISHERIES$CODE
  )
}

validate_fishery = function(fishery_code, field = "Fishery") {
  fishery_code = check_mandatory(trim(fishery_code), field)

  fisheries = fisheries_for(fishery_code)

  if(nrow(fisheries) == 0) stop(paste0("Unable to identify any valid fishery by code '", fishery_code, "'. Please refer to ", reference_codes("fisheries", "fisheries"), " for a list of valid fishery codes"))
  if(nrow(fisheries) >  1) stop(paste0("Multiple fisheries identified by code '", fishery_code, "'")) # This should never happen...

  return(fisheries)
}

is_multiple_gear_fishery = function(fishery_code) {
  fisheries = data.table(CODE = fishery_code)
  fisheries = merge(fisheries,
                    iotc.data.reference.codelists::LEGACY_FISHERIES[, .(CODE, IS_AGGREGATE)],
                    by = "CODE",
                    all.x = TRUE,
                    sort = FALSE)

  return(
    fisheries$IS_AGGREGATE
  )
}

fishery_type_for = function(fishery_code) {
  fisheries = data.table(CODE = fishery_code)
  fisheries = merge(fisheries,
                    iotc.data.reference.codelists::LEGACY_FISHERIES[, .(CODE, FISHERY_TYPE_CODE)],
                    by = "CODE",
                    all.x = TRUE,
                    sort = FALSE)

  return(
    fisheries$FISHERY_TYPE_CODE
  )
}

## IOTC MAIN AREAS

IOTC_main_areas_for = function(IOTC_main_area_code) {
  return(
    iotc.data.reference.codelists::IOTC_MAIN_AREAS[CODE %in% trim(IOTC_main_area_code)]
  )
}

is_IOTC_main_area_valid = function(IOTC_main_area_code) {
  return(
    trim(IOTC_main_area_code) %in% iotc.data.reference.codelists::IOTC_MAIN_AREAS$CODE
  )
}

validate_IOTC_main_area = function(IOTC_main_area_code, field = "IOTC main area") {
  IOTC_main_area_code = check_mandatory(trim(IOTC_main_area_code), field)

  IOTC_main_areas = IOTC_main_areas_for(IOTC_main_area_code)

  if(nrow(IOTC_main_areas) == 0) stop(paste0("Unable to identify any valid IOTC main area by code '", IOTC_main_area_code, "'. Please refer to ", reference_codes("admin", "IOTCareasMain"), " for a list of valid IOTC main area codes"))
  if(nrow(IOTC_main_areas) >  1) stop(paste0("Multiple IOTC main areas identified by code '", IOTC_main_area_code, "'")) # This should never happen...

  return(IOTC_main_areas)
}

## GRIDS CE-SF

grids_CE_SF_for = function(grid_code) {
  return(
    iotc.data.reference.codelists::IOTC_GRIDS_CE_SF[CODE %in% trim(grid_code)]
  )
}

is_grid_CE_SF_valid = function(grid_code) {
  return(
    trim(grid_code) %in% iotc.data.reference.codelists::IOTC_GRIDS_CE_SF$CODE
  )
}

validate_grid_CE_SF = function(grid_code, field = "Grid") {
  grid_code = check_mandatory(trim(grid_code), field)

  grids = grids_CE_SF_for(grid_code)

  if(nrow(grids) == 0) stop(paste0("Unable to identify any valid IOTC grid (1x1 or 5x5) by code '", grid_code, "'. Please refer to ", reference_codes("admin", "IOTCgridsCESF"), " for a list of valid IOTC (1x1 and 5x5) grid codes"))
  if(nrow(grids) >  1) stop(paste0("Multiple IOTC grids (1x1 or 5x5) identified by code '", grid_code, "'")) # This should never happen...

  return(grids)
}

## GRIDS AR

grids_AR_for = function(grid_code) {
  return(
    iotc.data.reference.codelists::IOTC_GRIDS_AR[CODE %in% trim(grid_code)]
  )
}

is_grid_AR_valid = function(grid_code) {
  return(
    trim(grid_code) %in% iotc.data.reference.codelists::IOTC_GRIDS_AR$CODE
  )
}

validate_grid_AR = function(grid_code, field = "Grid") {
  grid_code = check_mandatory(trim(grid_code), field)

  grids = grids_AR_for(grid_code)

  if(nrow(grids) == 0) stop(paste0("Unable to identify any valid IOTC grid (1x1 or 5x5) or artisanal area by code '", grid_code, "'. Please refer to ", reference_codes("admin", "IOTCgridsAR"), " for a list of valid IOTC (1x1 and 5x5) grid and artisanal area codes"))
  if(nrow(grids) >  1) stop(paste0("Multiple IOTC grids (1x1 or 5x5) or artisanal areas identified by code '", grid_code, "'")) # This should never happen...

  return(grids)
}

## SPECIES

species_for = function(species_code) {
  return(
    iotc.data.reference.codelists::LEGACY_SPECIES[CODE %in% trim(species_code)]
  )
}

is_species_valid = function(species_code) {
  return(
    trim(species_code) %in% iotc.data.reference.codelists::LEGACY_SPECIES$CODE
  )
}

validate_species = function(species_code, field = "Species") {
  species_code = check_mandatory(trim(species_code), field)

  species = species_for(species_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid species by code '", species_code, "'. Please refer to ", reference_codes("biological", "allSpecies"), " for a list of valid species codes"))
  if(nrow(species) >  1) stop(paste0("Multiple species identified by code '", species_code, "'")) # This should never happen...

  return(species)
}

is_species_aggregate = function(species_code) {
  species = data.table(CODE = species_code)
  species = merge(species,
                  iotc.data.reference.codelists::LEGACY_SPECIES[, .(CODE, IS_AGGREGATE)],
                  by = "CODE",
                  all.x = TRUE,
                  sort = FALSE)

  return(
    species$IS_AGGREGATE
  )
}

## DATA TYPES

data_type_for = function(data_type_code) {
  return(
    iotc.data.reference.codelists::DATA_TYPES[CODE %in% trim(data_type_code)]
  )
}

is_data_type_valid = function(data_type_code) {
  return(
    trim(data_type_code) %in% iotc.data.reference.codelists::DATA_TYPES$CODE
  )
}

validate_data_type = function(data_type_code, field = "Data type") {
  data_type_code = check_mandatory(trim(data_type_code), field)

  data_types = data_types_for(data_type_code)

  if(nrow(species) == 0) stop(paste0("Unable to identify any valid data type by code '", data_type_code, "'. Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes"))
  if(nrow(species) >  1) stop(paste0("Multiple data types identified by code '", data_type_code, "'")) # This should never happen...

  return(data_types)
}

## DATA SOURCES

data_source_for = function(dataset_code, data_source_code) {
  dataset_code     = trim(dataset_code)
  data_source_code = trim(data_source_code)

  return(
    iotc.data.reference.codelists::DATA_SOURCES[paste0(dataset_code, "-", data_source_code) %in% paste0(DATASET_CODE, "-", CODE)]
  )
}

is_data_source_valid = function(dataset_code, data_source_code) {
  return(
    paste0(trim(dataset_code), "-", trim(data_source_code)) %in% paste0(iotc.data.reference.codelists::DATA_SOURCES$DATASET_CODE, "-",iotc.data.reference.codelists::DATA_SOURCES$CODE)
  )
}

validate_data_source = function(dataset_code, data_source_code, field = "Data source") {
  dataset_code     = check_mandatory(trim(dataset_code), "Dataset")
  data_source_code = check_mandatory(trim(data_source_code), field)

  data_sources = data_source_for(dataset_code, data_source_code)

  if(nrow(data_sources) == 0) stop(paste0("Unable to identify any valid data source by dataset '", dataset_code, "' and code '", data_source_code, "'. Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes"))
  if(nrow(data_sources) >  1) stop(paste0("Multiple data sources identified by dataset '", dataset_code, "' and code '", data_source_code, "'")) # This should never happen...

  return(data_sources)
}

## DATA PROCESSINGS

data_processing_for = function(dataset_code, data_processing_code) {
  dataset_code         = trim(dataset_code)
  data_processing_code = trim(data_processing_code)

  return(
    iotc.data.reference.codelists::DATA_PROCESSINGS[paste0(dataset_code, "-", data_processing_code) %in% paste0(DATASET_CODE, "-", CODE)]
  )
}

is_data_processing_valid = function(dataset_code, data_processing_code) {
  return(
    paste0(trim(dataset_code), "-", trim(data_processing_code)) %in% paste0(iotc.data.reference.codelists::DATA_PROCESSINGS$DATASET_CODE, "-",iotc.data.reference.codelists::DATA_PROCESSINGS$CODE)
  )
}

validate_data_processing = function(dataset_code, data_processing_code, field = "Data processing") {
  dataset_code         = check_mandatory(trim(dataset_code), "Dataset")
  data_processing_code = check_mandatory(trim(data_processing_code), field)

  data_processings = data_processing_for(dataset_code, data_processing_code)

  if(nrow(data_processings) == 0) stop(paste0("Unable to identify any valid data processing by dataset '", dataset_code, "' and code '", data_processing_code, "'. Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes"))
  if(nrow(data_processings) >  1) stop(paste0("Multiple data processings identified by dataset '", dataset_code, "' and code '", data_processing_code, "'")) # This should never happen...

  return(data_processings)
}

## DATA COVERAGE TYPES

data_coverage_type_for = function(data_coverage_type_code) {
  return(
    iotc.data.reference.codelists::DATA_COVERAGE_TYPES[CODE %in% trim(data_coverage_type_code)]
  )
}

is_data_coverage_type_valid = function(data_coverage_type_code) {
  return(
    trim(data_coverage_type_code) %in% iotc.data.reference.codelists::DATA_COVERAGE_TYPES$CODE
  )
}

validate_data_coverage_type = function(data_coverage_type_code, field = "Data coverage type") {
  data_coverage_type_code = check_mandatory(trim(data_coverage_type_code), field)

  data_coverages = data_coverage_type_for(data_coverage_type_code)

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data coverage type by code '", data_coverage_type_code, "'. Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid coverage type codes"))
  if(nrow(item) >  1) stop(paste0("Multiple data coverage types identified by code '", data_coverage_type_code, "'")) # This should never happen...

  return(data_coverages)
}

## DATA ESTIMATIONS

data_estimation_for = function(data_estimation_code) {
  return(
    iotc.data.reference.codelists::DATA_ESTIMATIONS[CODE %in% trim(data_estimation_code)]
  )
}

is_data_estimation_valid = function(data_estimation_code) {
  return(
    trim(data_estimation_code) %in% iotc.data.reference.codelists::DATA_ESTIMATIONS$CODE
  )
}

validate_data_estimation = function(data_estimation_code, field = "Data estimation") {
  data_estimation_code = check_mandatory(trim(data_estimation_code), field)

  data_estimations = data_estimation_for(data_estimation_code)

  if(nrow(item) == 0) stop(paste0("Unable to identify any valid data estimation type by code '", data_estimation_code, "'. Please refer to ", reference_codes("data", "estimations"), " for a list of valid data estimation codes"))
  if(nrow(item) >  1) stop(paste0("Multiple data estimation types identified by code '", data_estimation_code, "'")) # This should never happen...

  return(data_estimations)
}

## FATE TYPES

fate_type_for = function(fate_type_code) {
  return(
    iotc.data.reference.codelists::TYPES_OF_FATE[CODE %in% trim(fate_type_code)]
  )
}

is_fate_type_valid = function(fate_type_code) {
  return(
    trim(fate_type_code) %in% iotc.data.reference.codelists::TYPES_OF_FATE$CODE
  )
}

validate_fate_type = function(fate_type_code, field = "Fate type") {
  fate_type_code = check_mandatory(trim(fate_type_code), field)

  fate_types = fate_type_for(fate_type_code)

  if(nrow(fate_types) == 0) stop(paste0("Unable to identify any valid fate type by code '", fate_type_code, "'. Please refer to ", reference_codes("biological", "typesOfFate"), " for a list of valid fate type codes"))
  if(nrow(fate_types) >  1) stop(paste0("Multiple fate types identified by code '", fate_type_code, "'")) # This should never happen...

  return(fate_types)
}

## FATES

fate_for = function(type_of_fate_code, fate_code) {
  type_of_fate_code = trim(type_of_fate_code)
  fate_code         = trim(fate_code)

  return(
    iotc.data.reference.codelists::FATES[paste0(type_of_fate_code, "-", fate_code) %in% paste0(TYPE_OF_FATE_CODE, "-", CODE)]
  )
}

is_fate_valid = function(type_of_fate_code, fate_code) {
  return(
    paste0(trim(type_of_fate_code), "-", trim(fate_code)) %in% paste0(iotc.data.reference.codelists::FATES$TYPE_OF_FATE_CODE, "-", iotc.data.reference.codelists::FATES$CODE)
  )
}

validate_fate = function(type_of_fate_code, fate_code, field = "Fate") {
  type_of_fate_code = check_mandatory(trim(type_of_fate_code), "Type of fate")
  fate_code         = check_mandatory(trim(fate_code), field)

  fates = fate_for(type_of_fate_code, fate_code)

  if(nrow(fates) == 0) stop(paste0("Unable to identify any valid fate by type of fate '", type_of_fate_code, "' and code '", fate_code, "'. Please refer to ", reference_codes("biological", "fates"), " for a list of valid fate codes"))
  if(nrow(fates) >  1) stop(paste0("Multiple fates identified by type of fate '", type_of_fate_code, "' and code '", fate_code, "'")) # This should never happen...

  return(fates)
}

## RETAIN REASONS

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

## DISCARD REASONS

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

## DATA RAISINGS

data_raising_for = function(data_raising_code) {
  return(
    iotc.data.reference.codelists::DATA_RAISINGS[CODE %in% trim(data_raising_code)]
  )
}

is_data_raising_valid = function(data_raising_code) {
  return(
    trim(data_raising_code) %in% iotc.data.reference.codelists::DATA_RAISINGS$CODE
  )
}

validate_data_raising = function(data_raising_code, field = "Data raising") {
  data_raising_code = check_mandatory(trim(data_raising_code), field)

  data_raisings = data_raising_for(data_raising_code)

  if(nrow(data_raisings) == 0) stop(paste0("Unable to identify any valid data raising by code '", data_raising_code, "'. Please refer to ", reference_codes("data", "raisings"), " for a list of valid data raising codes"))
  if(nrow(data_raisings) >  1) stop(paste0("Multiple data raisings identified by code '", data_raising_code, "'")) # This should never happen...

  return(data_raisings)
}

## CONDITIONS

condition_for = function(condition_code) {
  return(
    iotc.data.reference.codelists::CONDITIONS[CODE %in% trim(condition_code)]
  )
}

is_condition_valid = function(condition_code) {
  return(
    trim(condition_code) %in% iotc.data.reference.codelists::CONDITIONS$CODE
  )
}

validate_condition = function(condition_code, field = "Condition") {
  condition_code = check_mandatory(trim(condition_code), field)

  conditions = condition_for(condition_code)

  if(nrow(conditions) == 0) stop(paste0("Unable to identify any valid condition by code '", condition_code, "'. Please refer to ", reference_codes("biological", "individualConditions"), " for a list of valid condition codes"))
  if(nrow(conditions) >  1) stop(paste0("Multiple conditions identified by code '", condition_code, "'")) # This should never happen...

  return(conditions)
}

## SEX

sex_for = function(sex_code) {
  return(
    iotc.data.reference.codelists::SEX[CODE %in% trim(sex_code)]
  )
}

is_sex_valid = function(sex_code) {
  return(
    trim(sex_code) %in% iotc.data.reference.codelists::SEX$CODE
  )
}

validate_sex = function(sex_code, field = "Sex") {
  sex_code = check_mandatory(trim(sex_code), field)

  sexes = sex_for(sex_code)

  if(nrow(sexes) == 0) stop(paste0("Unable to identify any valid sex by code '", sex_code, "'. Please refer to ", reference_codes("biological", "sex"), " for a list of valid sex codes"))
  if(nrow(sexes) >  1) stop(paste0("Multiple sexes identified by code '", sex_code, "'")) # This should never happen...

  return(sexes)
}

## CATCH UNITS

catch_unit_for = function(catch_unit_code) {
  return(
    iotc.data.reference.codelists::CATCH_UNITS[CODE %in% trim(catch_unit_code)]
  )
}

is_catch_unit_valid = function(catch_unit_code) {
  return(
    trim(catch_unit_code) %in% iotc.data.reference.codelists::CATCH_UNITS$CODE
  )
}

validate_catch_unit = function(catch_unit_code, field = "Catch unit") {
  catch_unit_code = check_mandatory(trim(catch_unit_code), field)

  catch_units = catch_unit_for(catch_unit_code)

  if(nrow(catch_units) == 0) stop(paste0("Unable to identify any valid catch unit by code '", catch_unit_code, "'. Please refer to ", reference_codes("fisheries", "catchUnits"), " for a list of valid catch unit codes"))
  if(nrow(catch_units) >  1) stop(paste0("Multiple catch units identified by code '", catch_unit_code, "'")) # This should never happen...

  return(catch_units)
}

## EFFORT UNITS

effort_unit_for = function(effort_unit_code) {
  return(
    iotc.data.reference.codelists::EFFORT_UNITS[CODE %in% trim(effort_unit_code)]
  )
}

is_effort_unit_valid = function(effort_unit_code) {
  return(
    trim(effort_unit_code) %in% iotc.data.reference.codelists::EFFORT_UNITS$CODE
  )
}

validate_effort_unit = function(effort_unit_code, field = "Effort unit") {
  effort_unit_code = check_mandatory(trim(effort_unit_code), field)

  effort_units = effort_unit_for(effort_unit_code)

  if(nrow(effort_units) == 0) stop(paste0("Unable to identify any valid effort unit by code '", effort_unit_code, "'. Please refer to ", reference_codes("fisheries", "effortUnits"), " for a list of valid effort unit codes"))
  if(nrow(effort_units) >  1) stop(paste0("Multiple effort units identified by code '", effort_unit_code, "'")) # This should never happen...

  return(effort_units)
}

## MEASUREMENT TYPES

measurement_type_for = function(measurement_type_code) {
  return(
    iotc.data.reference.codelists::TYPES_OF_MEASUREMENT[CODE %in% trim(measurement_type_code)]
  )
}

is_measurement_type_valid = function(measurement_type_code) {
  return(
    trim(measurement_type_code) %in% iotc.data.reference.codelists::TYPES_OF_MEASUREMENT$CODE
  )
}

validate_measurement_type = function(measurement_type_code, field = "Measurement type") {
  measurement_type_code = check_mandatory(trim(measurement_type_code), field)

  measurement_types = measurement_type_for(measurement_type_code)

  if(nrow(measurement_types) == 0) stop(paste0("Unable to identify any valid measurement type by code '", measurement_type_code, "'. Please refer to ", reference_codes("biological", "typesOfMeasurement"), " for a list of valid measurement type codes"))
  if(nrow(measurement_types) >  1) stop(paste0("Multiple measurement types identified by code '", measurement_type_code, "'")) # This should never happen...

  return(measurement_types)
}

## MEASUREMENTS

measurement_for = function(measurement_type_code, measurement_code) {
  measurement_type_code = trim(measurement_type_code)
  measurement_code      = trim(measurement_code)

  return(
    iotc.data.reference.codelists::MEASUREMENT_TYPES[paste0(measurement_type_code, "-", measurement_code) %in% paste0(TYPE_OF_MEASUREMENT_CODE, "-", CODE)]
  )
}

is_measurement_valid = function(measurement_type_code, measurement_code) {
  return(
    paste0(trim(measurement_type_code), "-", trim(measurement_code)) %in% paste0(iotc.data.reference.codelists::MEASUREMENT_TYPES$TYPE_OF_MEASUREMENT_CODE, "-", iotc.data.reference.codelists::MEASUREMENT_TYPES$CODE)
  )
}

validate_measurement = function(measurement_type_code, measurement_code, field = "Measurement") {
  measurement_type_code = check_mandatory(trim(measurement_type_code), "Type of measurement")
  measurement_code      = check_mandatory(trim(measurement_code), field)

  measurements = measurement_for(measurement_type_code, measurement_code)

  if(nrow(measurements) == 0) stop(paste0("Unable to identify any valid measurement by type of measurement '", measurement_type_code, "' and code '", measurement_code, "'. Please refer to ", reference_codes("biological", "allMeasurementTypes"), " for a list of valid measurement codes"))
  if(nrow(measurements) >  1) stop(paste0("Multiple measurements identified by type of measurement '", measurement_type_code, "' and code '", measurement_code, "'")) # This should never happen...

  return(measurements)
}

## MEASURING TOOLS

measuring_tool_for = function(measurement_type_code, measuring_tool_code) {
  measurement_type_code = trim(measurement_type_code)
  measuring_tool_code   = trim(measuring_tool_code)

  return(
    iotc.data.reference.codelists::MEASUREMENT_TOOLS[paste0(measurement_type_code, "-", measuring_tool_code) %in% paste0(TYPE_OF_MEASUREMENT_CODE, "-", CODE)]
  )
}

is_measuring_tool_valid = function(measurement_type_code, measuring_tool_code) {
  return(
    paste0(trim(measurement_type_code), "-", trim(measuring_tool_code)) %in% paste0(iotc.data.reference.codelists::MEASUREMENT_TOOLS$TYPE_OF_MEASUREMENT_CODE, "-", iotc.data.reference.codelists::MEASUREMENT_TOOLS$CODE)
  )
}

validate_measuring_tool = function(measurement_type_code, measuring_tool_code, field = "Measuring tool") {
  measurement_type_code = check_mandatory(trim(measurement_type_code), "Type of measurement")
  measuring_tool_code   = check_mandatory(trim(measuring_tool_code), field)

  measuring_tools = measuring_tool_for(measurement_type_code, measuring_tool_code)

  if(nrow(measuring_tools) == 0) stop(paste0("Unable to identify any valid measuring tool by type of measurement '", measurement_type_code, "' and code '", measuring_tool_code, "'. Please refer to ", reference_codes("biological", "allMeasurementTool"), " for a list of valid measuring tools"))
  if(nrow(measuring_tools) >  1) stop(paste0("Multiple measuring tools identified by type of measurement '", measurement_type_code, "' and code '", measuring_tool_code, "'")) # This should never happen...

  return(measuring_tools)
}
