extract_metadata_3CE_multiple = function(form) {
  metadata = common_metadata(form$form_metadata)

  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_3CE_multiple = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:11]
  colnames(strata) = c("MONTH", "FISHERY_CODE", "AREA_CODE", "ESTIMATION_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE", "DATA_RAISING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[2:nrow(form_data), 12:ncol(form_data)]

  species_codes    = unlist(lapply(records[1, 8:ncol(records[1])], trim), use.names = FALSE)
  catch_unit_codes = unlist(lapply(records[2, 8:ncol(records[2])], trim), use.names = FALSE)

  #colnames(records) = species_codes

  records = records[3:nrow(records)]

  efforts = records[, 1:6]
  colnames(efforts) = c("EFFORT_PRIMARY_CODE"  , "EFFORT_PRIMARY",
                        "EFFORT_SECONDARY_CODE", "EFFORT_SECONDARY",
                        "EFFORT_TERTIARY_CODE" , "EFFORT_TERTIARY")

  catches = records[, 8:ncol(records)]

  catches[, lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = list(
          species_codes    = species_codes,
          catch_unit_codes = catch_unit_codes
        ),
        data = list(
          efforts = efforts,
          catches = catches
        )
      )
    )
  )
}
