### METHODS TO HANDLE VALIDATION SUMMARY FOR COMMON ELEMENTS

# STRATA

report_strata = function(message_list, strata_validation) {
  # Generic INFO messages on strata composition

  if(strata_validation$total$number > 0) {
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$total$number,     " total strata"    )))
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$non_empty$number, " non-empty strata")))
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$unique$number,    " unique strata"   )))
  }

  # FATAL message on no strata

  if(strata_validation$total == 0) {
    message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0("Zero strata detected")))
  } else {
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
  }

  return(message_list)
}

# MONTHS
report_months = function(message_list, months_validation, column = "B") {
  if(months_validation$missing$number > 0) {
    if(months_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(months_validation$missing$number, " missing months")))

    for(row in months_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing month in row #", row)))
  }

  if(months_validation$invalid$number > 0) {
    if(months_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(months_validation$invalid$number, " invalid months provided. Please use 1 .. 12 for Jan .. Dec")))

    for(row in months_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid month in row #", row)))
  }

  return(message_list)
}

# FISHERIES
report_fisheries = function(message_list, fisheries_validation, column) {
  if(fisheries_validation$aggregates$number > 0) {
    message_list = add(message_list, new("Message", level = "WARN", source = "Data", column = column, text = paste0(fisheries_validation$aggregates$number, " aggregated fishery code(s) reported. Please refer to ", reference_codes("legacy", "fisheries"), " for a list of valid legacy fishery codes")))

    for(row in fisheries_validation$aggregates$row_indexes) {
      message_list = add(message_list, new("Message", level = "WARN", source = "Data", column = column, row = row, text = paste0("Aggregated fishery code '", fisheries_validation$aggregates$codes[which(fisheries_validation$aggregates$row_indexes == row)], "' reported in row #", row)))
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
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid fishery code '", fisheries_validation$invalid$codes[which(fisheries_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# TARGET SPECIES
report_target_species = function(message_list, target_species_validation, column) {
  if(target_species_validation$missing$number > 0) {
    if(target_species_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(target_species_validation$missing$number, " missing target species code(s)")))

    for(row in target_species_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing target species code in row #", row)))
    }
  }

  if(target_species_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(target_species_validation$invalid$number, " invalid target species code(s) reported. Please refer to ", reference_codes("legacy", "species"), " for a list of valid legacy fishery codes")))

    for(row in target_species_validation$invalid$row_indexes) {
       message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid target species code '", target_species_validation$invalid$codes[which(target_species_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# IOTC AREA
report_IOTC_area = function(message_list, area_validation, column) {
  if(area_validation$missing$number > 0) {
    if(area_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(area_validation$missing$number, " missing IOTC main area codes")))

    for(row in area_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing IOTC main area code in row #", row)))
    }
  }

  if(area_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(area_validation$invalid$number, " invalid IOTC main area code(s) reported. Please refer to ", reference_codes("admin", "IOTCareasMain"), " for a list of valid IOTC main area codes")))

    for(row in area_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid IOTC main area code '", area_validation$invalid$codes[which(area_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# DATA TYPE
report_data_type = function(message_list, data_type_validation, column) {
  if(data_type_validation$missing$number > 0) {
    if(data_type_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_type_validation$missing$number, " missing data type codes")))

    for(row in data_type_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data type code in row #", row)))
    }
  }

  if(data_type_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_type_validation$invalid$number, " invalid data type code(s) reported. Please refer to ", reference_codes("data", "types"), " for a list of valid data type codes")))

    for(row in data_type_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data type code '", data_type_validation$invalid$codes[which(data_type_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# DATA SOURCE
report_data_source = function(message_list, data_source_validation, column) {
  if(data_source_validation$missing$number > 0) {
    if(data_source_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_source_validation$missing$number, " missing data source codes")))

    for(row in data_source_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data source code in row #", row)))
    }
  }

  if(data_source_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_source_validation$invalid$number, " invalid data source code(s) reported. Please refer to ", reference_codes("data", "sources"), " for a list of valid data source codes for this dataset")))

    for(row in data_source_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data source code '", data_source_validation$invalid$codes[which(data_source_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# DATA PROCESSING
report_data_processing = function(message_list, data_processing_validation, column) {
  if(data_processing_validation$missing$number > 0) {
    if(data_processing_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_processing_validation$missing$number, " missing data processing codes")))

    for(row in data_processing_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing data processing code in row #", row)))
    }
  }

  if(data_processing_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(data_processing_validation$invalid$number, " invalid data processing code(s) reported. Please refer to ", reference_codes("data", "processings"), " for a list of valid data processing codes for this dataset")))

    for(row in data_processing_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid data processing code '", data_processing_validation$invalid$codes[which(data_processing_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# COVERAGE TYPE
report_coverage_type = function(message_list, coverage_type_validation, column) {
  if(coverage_type_validation$missing$number > 0) {
    if(coverage_type_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_type_validation$missing$number, " missing coverage type codes")))

    for(row in coverage_type_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Missing coverage type code in row #", row)))
    }
  }

  if(coverage_type_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(coverage_type_validation$invalid$number, " invalid coverage type code(s) reported. Please refer to ", reference_codes("data", "coverageTypes"), " for a list of valid data coverage types")))

    for(row in coverage_type_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, row = row, text = paste0("Invalid coverage type code '", coverage_type_validation$invalid$codes[which(coverage_type_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  return(message_list)
}

# COVERAGE VALUE
report_coverage_value = function(message_list, coverage_value_validation, column) {
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
}

# DATA
report_data = function(message_list, data_validation, allow_empty_records) {
  # Generic INFO messages on data composition

  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$total$number,     " total strata"    )))
  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$non_empty$number, " non-empty strata")))
  #message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(strata_validation$unique$number,    " unique strata"   )))

  # FATAL / WARNING message on no records

  if(data_validation$total == 0) {
    message_list = add(message_list, new("Message", level = ifelse(allow_empty_records, "WARN", "FATAL"), source = "Data", text = paste0("Zero data records detected")))
  } else {
    # FATAL messages on empty rows

    if(data_validation$empty_rows$number > 0 && !allow_empty_records) {
      if(data_validation$empty_rows$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(data_validation$empty_rows$number, " empty data records detected")))

      for(row in data_validation$empty_rows$row_indexes) {
        message_list = add(message_list, new("Message", level = "FATAL", source = "Data", row = row, text = paste0("Empty data record detected at row #", row)))
      }
    }

    # FATAL messages on empty columns

    if(data_validation$empty_columns$number > 0) {
      if(data_validation$empty_columns$number > 1) message_list = add(message_list, new("Message", level = "FATAL", source = "Data", text = paste0(data_validation$empty_columns$number, " empty data columns detected")))

      for(col in data_validation$empty_columns$col_indexes) {
        message_list = add(message_list, new("Message", level = "FATAL", source = "Data", column = col, text = paste0("Empty data column detected at column ", col)))
      }
    }
  }

  return(message_list)
}

# SPECIES
report_species = function(message_list, species_validation, species_row, species_domain = "legacy", species_codelist = "species") {
  if(species_validation$aggregates$number > 0) { # Aggregates
    message_list = add(message_list, new("Message", level = "WARN", source = "Data", row = species_row, text = paste0(species_validation$aggregates$number, " aggregated species code(s) reported. Please refer to ", reference_codes(species_domain, species_codelist), " for a list of valid species codes")))

    for(col in species_validation$aggregates$col_indexes)
      message_list = add(message_list, new("Message", level = "WARN", source = "Data", row = species_row, column = col, text = paste0("Aggregated species code '", species_validation$aggregates$codes[which(species_validation$aggregates$col_indexes == col)], "' reported in column ", col)))
  }

  if(species_validation$missing$number > 0) { # Missing
    if(species_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = species_row, text = paste0(species_validation$missing$number, " missing species codes")))

    for(col in species_validation$missing$col_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = species_row, column = col, text = paste0("Missing species code in column ", col)))
  }

  if(species_validation$invalid$number > 0) { # Invalid
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = species_row, text = paste0(species_validation$invalid$number, " invalid species code(s) reported. Please refer to ", reference_codes(species_domain, species_codelist), " for a list of valid species codes")))

    for(col in species_validation$invalid$col_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = species_row, column = col, text = paste0("Invalid species code '", species_validation$invalid$codes[which(species_validation$invalid$col_indexes == col)], "' in column ", col)))
  }

  return(message_list)
}

# SPECIES (as column, e.g. 4-SF-multiple)
report_species_column = function(message_list, species_validation, column = "F") {
  if(species_validation$missing$number > 0) {
    if(species_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", text = paste0(species_validation$missing$number, " missing species codes")))

    for(row_index in species_validation$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = column, text = paste0("Missing species code in row #", row_index)))
    }
  }

  if(species_validation$invalid$number > 0) {
    if(species_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", text = paste0(species_validation$invalid$number, " invalid species codes")))

    for(row_index in species_validation$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = column, text = paste0("Invalid species code '", species_validation$invalid$codes[which(species_validation$invalid$row_indexes == row_index)], "' in row #", row_index)))
    }
  }

  if(species_validation$aggregates$number > 0) {
    if(species_validation$aggregates$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", text = paste0(species_validation$aggregates$number, " aggregate species codes")))

    for(row_index in species_validation$aggregates$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = as.integer(row_index), column = column, text = paste0("Aggregate species code '", species_validation$aggregates$codes[which(species_validation$aggregates$row_indexes == row_index)], "' in row #", row_index)))
    }
  }

  return(message_list)
}

# CATCHES (AS CELLS)
report_catches = function(message_list, catches_validation) {
  if(catches_validation$positive$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(catches_validation$positive$number, " positive catch value(s) reported")))

  if(catches_validation$na$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", text = paste0(catches_validation$na$number, " empty catch value(s) reported")))

  if(catches_validation$zero$number > 0)
    message_list = add(message_list, new("Message", level = "WARN", source = "Data", text = paste0(catches_validation$zero$number, " catch value(s) explicitly reported as zero: consider leaving the cells empty instead")))

  if(catches_validation$negative$number > 0) {
    if(catches_validation$negative$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", text = paste0(catches_validation$negative$number, " negative catch values reported")))

    for(n in 1:nrow(catches_validation$negative$cells)) {
      cell = catches_validation$negative$cells[n]

      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = cell$ROW, column = cell$COL, text = paste0("Negative catch value reported in cell ", cell$INDEXES)))
    }
  }

  if(catches_validation$non_num$number > 0) {
    if(catches_validation$non_num$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", text = paste0(catches_validation$non_num$number, " non-numeric catch values reported")))

    for(n in 1:nrow(catches_validation$non_num$cells)) {
      cell = catches_validation$non_num$cells[n]

      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = cell$ROW, column = cell$COL, text = paste0("Non-numeric catch value reported in cell ", cell$INDEXES)))
    }
  }

  return(message_list)
}

# EFFORTS

## 3CE - single
report_effort_values = function(message_list, effort_value_validation, type = "primary", column = "E") {
  if(effort_value_validation$unit_provided) {
    if(effort_value_validation$missing$number > 0) {
      if(effort_value_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(effort_value_validation$missing$number, " missing ", type, " effort values")))

      for(row in effort_value_validation$missing$row_indexes) {
        message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing ", type, " effort value in row #", row)))
      }
    }

    if(effort_value_validation$invalid$number > 0) {
      if(effort_value_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(effort_value_validation$invalid$number, " invalid ", type, " effort values")))

      for(row in effort_value_validation$invalid$row_indexes) {
        message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid ", type, " effort value in row #", row)))
      }
    }
  } else {
    if(effort_value_validation$values_provided) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0("Effort values provided, but no ", type, " effort unit is found in the metadata")))
    }
  }

  return(message_list)
}

## 3CE - multiple
report_effort_multiple = function(message_list, effort_validation, type = "primary", column_code = "M", column_value = "N") {
  # CODES

  if(effort_validation$code$missing$number > 0) {
    if(effort_validation$value$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column_code, text = paste0(effort_validation$code$missing$number, " missing ", type, " effort codes")))

    for(row in effort_validation$code$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column_value, text = paste0("Missing ", type, " effort code in row #", row)))
    }
  }

  if(effort_validation$code$invalid$number > 0) {
    if(effort_validation$value$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column_code, text = paste0(effort_validation$code$missing$number, " invalid ", type, " effort codes. Please refer to ", reference_codes("fishery", "effortUnits"), " for a list of valid effort unit codes")))

    for(row in effort_validation$code$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column_code, text = paste0("Invalid ", type, " effort code '", effort_validation$invalid$codes[which(effort_validation$invalid$row_indexes == row)], "' in row #", row)))
    }
  }

  # VALUES

  if(effort_validation$value$missing$number > 0) {
    if(effort_validation$value$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column_value, text = paste0(effort_validation$value$missing$number, " missing ", type, " effort values")))

    for(row in effort_validation$value$missing$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column_value, text = paste0("Missing ", type, " effort value in row #", row)))
    }
  }

  if(effort_validation$value$invalid$number > 0) {
    if(effort_validation$value$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column_value, text = paste0(effort_validation$value$invalid$number, " invalid ", type, " effort values")))

    for(row in effort_validation$value$invalid$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column_value, text = paste0("Invalid ", type, " effort value in row #", row)))
    }
  }

  return(message_list)
}

# 4-SF - SEX
report_sex = function(message_list, sex_validation, column = "D") {
  if(sex_validation$missing$number > 0) {
    if(sex_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(sex_validation$missing$number, " missing sex codes")))

    for(row in sex_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing sex code in row #", row)))
  }

  if(sex_validation$invalid$number > 0) {
    message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(sex_validation$invalid$number, " invalid sex code(s) reported. Please refer to ", reference_codes("biological", "sex"), " for a list of valid measuring sex codes")))

    for(row in sex_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid sex code '", sex_validation$invalid$codes[which(sex_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - multiple - TYPE OF FATE
report_type_of_fate = function(message_list, type_of_fate_validation, column = "H") {
  if(type_of_fate_validation$missing$number > 0) {
    if(type_of_measure_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(type_of_fate_validation$missing$number, " missing type of fate(s)")))

    for(row in type_of_fate_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing type of fate in row #", row)))
  }

  if(type_of_fate_validation$invalid$number > 0) {
    if(type_of_fate_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(type_of_fate_validation$invalid$number, " invalid type of fate(s). Please refer to ", reference_codes("biological", "typesOfFate"), " for a list of valid type of fate codes")))

    for(row in type_of_fate_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid type of fate '", type_of_fate_validation$invalid$codes[which(type_of_fate_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - multiple - FATE
report_fate = function(message_list, fate_validation, column = "I") {
  if(fate_validation$missing$number > 0) {
    if(fate_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(fate_validation$missing$number, " missing fate(s)")))

    for(row in fate_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing fate in row #", row)))
  }

  if(fate_validation$invalid$number > 0) {
    if(fate_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(fate_validation$invalid$number, " invalid fate(s). Please refer to ", reference_codes("biological", "fates"), " for a list of valid fate codes")))

    for(row in fate_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid fate '", fate_validation$invalid$codes[which(fate_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - multiple - TYPE OF MEASURE
report_type_of_measure = function(message_list, type_of_measure_validation, column = "Q") {
  if(type_of_measure_validation$missing$number > 0) {
    if(type_of_measure_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(type_of_measure_validation$missing$number, " missing type of measure(s)")))

    for(row in type_of_measure_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing type of measure in row #", row)))
  }

  if(type_of_measure_validation$invalid$number > 0) {
    if(type_of_measure_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(type_of_measure_validation$invalid$number, " invalid type of measure(s). Please refer to ", reference_codes("biological", "typesOfMeasurement"), " for a list of valid type of measure codes")))

    for(row in type_of_measure_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid type of measure '", type_of_measure_validation$invalid$codes[which(type_of_measure_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - multiple - MEASURE TYPE
report_measure_type = function(message_list, measure_type_validation, column = "R") {
  if(measure_type_validation$missing$number > 0) {
    if(measure_type_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(measure_type_validation$missing$number, " missing measure type(s)")))

    for(row in measure_type_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing measure type in row #", row)))
  }

  if(measure_type_validation$invalid$number > 0) {
    if(measure_type_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(measure_type_validation$invalid$number, " invalid measure type(s). Please refer to ", reference_codes("biological", "allMeasurementTypes"), " for a list of valid measure type codes")))

    for(row in measure_type_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid measure type '", measure_type_validation$invalid$codes[which(measure_type_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - multiple - MEASURING TOOL
report_measuring_tool = function(message_list, measuring_tool_validation, column = "S") {
  if(measuring_tool_validation$missing$number > 0) {
    if(measuring_tool_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(measuring_tool_validation$missing$number, " missing measuring tool(s)")))

    for(row in measuring_tool_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing measuring tool in row #", row)))
  }

  if(measuring_tool_validation$invalid$number > 0) {
    if(measuring_tool_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(measuring_tool_validation$invalid$number, " invalid measuring tool(s). Please refer to ", reference_codes("biological", "allMeasurementTools"), " for a list of valid measuring tool codes")))

    for(row in measuring_tool_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid measuring tool '", measuring_tool_validation$invalid$codes[which(measuring_tool_validation$invalid$row_indexes == row)], "' in row #", row)))
  }

  return(message_list)
}

# 4SF - SIZE CLASS
report_size_class = function(message_list, size_class_validation, column = "F") {
  if(size_class_validation$missing$number > 0) {
    if(size_class_validation$missing$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(size_class_validation$missing$number, " missing size class(es)")))

    for(row in size_class_validation$missing$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Missing size class in row #", row)))
  }

  if(size_class_validation$invalid$number > 0) {
    if(size_class_validation$invalid$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(size_class_validation$invalid$number, " invalid size class(es). Please ensure that size classes are numeric and greater than zero")))

    for(row in size_class_validation$invalid$row_indexes)
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row, column = column, text = paste0("Invalid size class in row #", row)))
  }

  return(message_list)
}

# 4SF - NUMBER OF SAMPLES
report_number_of_samples = function(message_list, number_of_samples_validation, column = "G") {
  if(number_of_samples_validation$positive$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", column = column, text = paste0(number_of_samples_validation$positive$number, " positive value(s) reported as number of samples")))

  if(number_of_samples_validation$zero$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", column = column, text = paste0(number_of_samples_validation$zero$number, " number of samples explicitly reported as zero")))

  if(number_of_samples_validation$na$number > 0) {
    if(number_of_samples_validation$na$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_samples_validation$na$number, " empty value(s) reported as number of samples")))

    for(row_index in number_of_samples_validation$na$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Empty value reported as number of samples in row #", row_index)))
    }
  }

  if(number_of_samples_validation$negative$number > 0) {
    if(number_of_samples_validation$negative$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_samples_validation$negative$number, " negative value(s) reported as number of samples")))

    for(row_index in number_of_samples_validation$negative$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Negative value reported as number of samples in row #", row_index)))
    }
  }

  if(number_of_samples_validation$non_num$number > 0) {
    if(number_of_samples_validation$non_num$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_samples_validation$non_num$number, " non-numeric value(s) reported as number of samples")))

    for(row_index in number_of_samples_validation$non_num$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Non-numeric value reported as number of samples in row #", row_index)))
    }
  }

  return(message_list)
}

# 4SF - NUMBER OF FISH
report_number_of_fish = function(message_list, number_of_fish_validation, column = "H") {
  if(number_of_fish_validation$positive$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", column = column, text = paste0(number_of_fish_validation$positive$number, " positive value(s) reported as number of fish")))

  if(number_of_fish_validation$zero$number > 0)
    message_list = add(message_list, new("Message", level = "INFO", source = "Data", column = column, text = paste0(number_of_fish_validation$zero$number, " number of fish explicitly reported as zero")))

  if(number_of_fish_validation$na$number > 0) {
    if(number_of_fish_validation$na$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_fish_validation$na$number, " empty value(s) reported as number of fish")))

    for(row_index in number_of_fish_validation$na$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Empty value reported as number of fish in row #", row_index)))
    }
  }

  if(number_of_fish_validation$negative$number > 0) {
    if(number_of_fish_validation$negative$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_fish_validation$negative$number, " negative value(s) reported as number of fish")))

    for(row_index in number_of_fish_validation$negative$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Negative value reported as number of fish in row #", row_index)))
    }
  }

  if(number_of_fish_validation$non_num$number > 0) {
    if(number_of_fish_validation$non_num$number > 1) message_list = add(message_list, new("Message", level = "ERROR", source = "Data", column = column, text = paste0(number_of_fish_validation$non_num$number, " non-numeric value(s) reported as number of fish")))

    for(row_index in number_of_fish_validation$non_num$row_indexes) {
      message_list = add(message_list, new("Message", level = "ERROR", source = "Data", row = row_index, column = column, text = paste0("Non-numeric value reported as number of fish in row #", row_index)))
    }
  }

  return(message_list)
}
