extract_metadata_3CE = function(form) {
  metadata = common_metadata(form$form_metadata)

  fishery = trim(as.character(form$form_metadata[16, 7]))

  type_of_data    = trim(as.character(form$form_metadata[23, 4]))
  data_source     = trim(as.character(form$form_metadata[25, 4]))
  data_processing = trim(as.character(form$form_metadata[26, 4]))
  data_raising    = trim(as.character(form$form_metadata[27, 4]))
  coverage_type   = trim(as.character(form$form_metadata[28, 4]))
  coverage_value  = trim(as.character(form$form_metadata[29, 4]))
  coverage_value  = as.numeric(coverage_value)

  effort_primary    = trim(as.character(form$form_metadata[23, 7]))
  effort_secondary  = trim(as.character(form$form_metadata[24, 7]))
  effort_tertiary   = trim(as.character(form$form_metadata[25, 7]))

  catch_unit        = trim(as.character(form$form_metadata[27, 7]))

  metadata$comment = comments(form$form_metadata, 34)

  metadata$general_information$fishery = fishery

  metadata$data_specifications = list(
    type_of_data    = type_of_data,
    data_source     = data_source,
    data_processing = data_processing,
    data_raising    = data_raising,
    coverage_type   = coverage_type,
    coverage_value  = coverage_value,
    effort_units = list(
      primary   = effort_primary,
      secondary = effort_secondary,
      tertiary  = effort_tertiary
    ),
    catch_unit = catch_unit
  )

  return(metadata)
}

extract_strata_and_records_3CE = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:4]
  colnames(strata) = c("MONTH", "GRID_CODE", "ESTIMATION_CODE")

  records = form_data[3:nrow(form_data), 5:ncol(form_data)]

  species_codes    = unlist(lapply(records[1, 4:ncol(records)], trim), use.names = FALSE)

  #colnames(records) = species_codes

  records = records[2:nrow(records), lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = list(
          species_codes    = species_codes
        ),
        data = list(
          efforts = records[, 1:3],
          catches = records[, 4:ncol(records)]
        )
      )
    )
  )
}
