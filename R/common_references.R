### COMMON REFERENCE DATA MANAGEMENT

load_codelist = function(codelist_domain, codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  if(is.null(columns)) columns = "*"
  else columns = paste0(columns, collapse = ", ")
  return(
    query(
      connection,
      paste0("SELECT ", columns, " FROM [", codelist_domain, "].[", codelist_name, "]")
    )
  )
}

admin_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_admin", codelist_name, columns, connection))
}

gis_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_gis", codelist_name, columns, connection))
}

fishery_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_fishery", codelist_name, columns, connection))
}

biological_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_biological", codelist_name, columns, connection))
}

data_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_data", codelist_name, columns, connection))
}

legacy_domain = function(codelist_name, columns = NULL, connection = DB_IOTC_MASTER()) {
  return(load_codelist("refs_legacy", codelist_name, columns, connection))
}

### ADMIN REFERENCES

IN_ENTITIES  = admin_domain("ENTITIES")
IN_COUNTRIES = admin_domain("COUNTRIES")

IN_FLEETS       = admin_domain("FLEETS")
IN_FLEETS_FLAGS = admin_domain("FLEET_TO_FLAGS_AND_FISHERIES")
IN_FLEETS_FLAGS = merge(IN_FLEETS_FLAGS, IN_FLEETS, by.x = "FLEET_CODE", by.y = "CODE")

### DATA REFERENCES

IN_DATA_TYPES       = data_domain("TYPES")
IN_DATA_RAISINGS    = data_domain("RAISINGS")

# These shall be further specialized by type of dataset...
IN_DATA_SOURCES     = data_domain("SOURCES")
IN_DATA_PROCESSINGS = data_domain("PROCESSINGS")
IN_DATA_ESTIMATIONS = data_domain("ESTIMATIONS")

IN_DATA_COVERAGE_TYPES = data_domain("COVERAGE_TYPES")

### GIS REFERENCES

IN_IOTC_AREA            = gis_domain("V_IOTC_MAIN_AREAS",  columns = c("CODE", "NAME_EN", "NAME_FR"))
IN_IOTC_GRIDS_AR        = gis_domain("V_IOTC_GRIDS_AR",    columns = c("CODE", "NAME_EN", "NAME_FR"))
IN_IOTC_GRIDS_CE_SF     = gis_domain("V_IOTC_GRIDS_CE_SF", columns = c("CODE", "NAME_EN", "NAME_FR"))

IN_IOTC_GRIDS_01x01     = gis_domain("V_IOTC_GRIDS_01x01", columns = c("CODE", "NAME_EN", "NAME_FR"))
IN_IOTC_GRIDS_05x05     = gis_domain("V_IOTC_GRIDS_05x05", columns = c("CODE", "NAME_EN", "NAME_FR"))

### FISHERY REFERENCES

IN_FISHERIES            = fishery_domain("FISHERIES")

IN_CATCH_UNITS          = fishery_domain("CATCH_UNITS")
IN_DISCARD_UNITS        = IN_CATCH_UNITS
IN_EFFORT_UNITS         = fishery_domain("EFFORT_UNITS")

IN_BOAT_TYPES           = fishery_domain("BOAT_TYPES")
IN_BOAT_CLASS_TYPES     = fishery_domain("BOAT_CLASS_TYPES")
IN_MECHANIZATION_TYPES  = fishery_domain("MECHANIZATION_TYPES")
IN_PRESERVATION_METHODS = fishery_domain("FISH_PRESERVATION_METHODS")
IN_PROCESSING_TYPES     = fishery_domain("FISH_PROCESSING_TYPES")

IN_FAD_TYPES            = fishery_domain("FAD_TYPES")
IN_FAD_ACTIVITY_TYPES   = fishery_domain("FAD_ACTIVITY_TYPES")
IN_FAD_OWNERSHIP_TYPES  = fishery_domain("FAD_OWNERSHIPS")

### BIOLOGICAL REFERENCES

IN_SPECIES              = biological_domain("V_SPECIES", columns = c("CODE", "NAME_EN", "NAME_FR", "NAME_SCIENTIFIC", "IS_IOTC", "IS_AGGREGATE"))
IN_SEX                  = biological_domain("SEX")

IN_TYPES_OF_FATE        = biological_domain("TYPES_OF_FATE")

IN_FATES                = biological_domain("FATES")

IN_DISCARD_REASONS      = biological_domain("V_DISCARD_REASONS")
IN_RETAIN_REASONS       = biological_domain("V_RETAIN_REASONS")
IN_CONDITIONS           = biological_domain("INDIVIDUAL_CONDITIONS")

IN_TYPES_OF_MEASUREMENT = biological_domain("TYPES_OF_MEASUREMENT")
IN_MEASUREMENT_TYPES    = biological_domain("MEASUREMENT_TYPES")
IN_MEASUREMENT_TOOLS    = biological_domain("MEASUREMENT_TOOLS")
