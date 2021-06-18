sanitize_code = function(code) {
  return(
    sanitize(
      str_replace(code, "([A-Z0-9]+)\\-(.+)", "\\1")
    )
  )
}

sanitize = function(value) {
  value = str_trim(value)

  return(ifelse(is.na(value) | value == "", NA, value))
}

sanitize_date = function(date) {
  if(is.na(date) | str_trim(date) == "") return("--")

  tryCatch({
    return(as.Date(date))
  }, error = function(msg) {
    message(paste0("Unable to parse date '", date, "': ", msg))

    return(date)
  })
}

check_and_remove_extra_rows = function(data, min_num_non_NA = 3, remove_rows = TRUE) {
  data[, non_NA := sum(!is.na(.SD)), by = 1:nrow(data)]

  empty_rows = nrow(data[non_NA < min_num_non_NA])

  if(empty_rows > 0) warning(paste0("Empty rows (n=", empty_rows, ") in dataset: potentially unwanted data at the end of the file?"))

  if(remove_rows) {
    data = data[non_NA >= min_num_non_NA]
  }

  data$non_NA = NULL

  return(data)
}

load_species = function() {
  return(
    unique(
      all_codes("SPECIES")[, .(CODE, NAME_LT, NAME_EN, IS_IOTC, IS_AGGREGATE, IS_SSI, IS_BAIT, WP_CODE, SPECIES_GROUP_CODE, SPECIES_CATEGORY_CODE, ASFIS_FAMILY, ASFIS_ORDER, IUCN_STATUS)]
    )
  )
}

load_gear_mappings = function() {
  GEAR_MAPPING_QUERY = "
    WITH MAPPED AS (
      SELECT
      F.NAME_EN AS IOTC_FISHERY_NAME,
      F.CODE AS IOTC_FISHERY_CODE,
      G.EngDescr AS GEAR_NAME,
      G.Acode GEAR_CODE
      FROM
      [IOTCStatistics].[dbo].cl_map_fisheries M
      LEFT JOIN
      [IOTDB].[dbo].cdeGears G
      ON
      M.LEGACY_CODE = G.ACode collate SQL_Latin1_General_CP1_CI_AS
      LEFT JOIN
      [IOTCStatistics].[dbo].cl_fisheries F
      ON
      M.CL_FISHERY_ID = F.ID
      WHERE G.ACode NOT LIKE '%OB'
    ), SAME AS (
      SELECT
      F.NAME_EN AS IOTC_FISHERY_NAME,
      F.CODE AS IOTC_FISHERY_CODE,
      G.EngDescr AS GEAR_NAME,
      G.ACode AS GEAR_CODE
      FROM
      [IOTDB].[dbo].cdeGears G
      LEFT JOIN
      [IOTCStatistics].[dbo].CL_FISHERIES F
      ON
      ( G.ACode = F.CODE collate SQL_Latin1_General_CP1_CI_AS ) OR
      ( G.EngDescr = F.NAME_EN collate SQL_Latin1_General_CP1_CI_AS )
    ), BOTH AS (
      SELECT IOTC_FISHERY_NAME, IOTC_FISHERY_CODE, GEAR_NAME, GEAR_CODE FROM MAPPED
      UNION ALL
      SELECT IOTC_FISHERY_NAME, IOTC_FISHERY_CODE, GEAR_NAME, GEAR_CODE FROM SAME
    )
    SELECT DISTINCT *
      FROM BOTH
    WHERE IOTC_FISHERY_CODE IS NOT NULL
    ORDER BY 1"

  GEAR_MAPPINGS = query(query = GEAR_MAPPING_QUERY)

  return(GEAR_MAPPINGS)
}

check_repeated_species = function(column_names) {
  species_column_names = data.table(NAME = column_names)
  species_column_names = species_column_names[, .(COUNT = .N), keyby = .(NAME)]

  repeated_species = species_column_names[COUNT > 1]$NAME

  if(length(repeated_species) > 0) {
    warning(paste0(length(repeated_species), " repeated species codes: ", paste(repeated_species, ",")))
  }

  return(repeated_species)
}

fix_repeated_species = function(data, repeated_species) {
  if(length(repeated_species) > 0) {
    for(species in repeated_species) {
      idx = which(colnames(data) == species)

      data[, idx[1] := rowSums(.SD), .SDcols = idx]

      for(i in (2:length(idx))) {
        data[, idx[i]] = NULL
      }
    }
  }

  return(data)
}
