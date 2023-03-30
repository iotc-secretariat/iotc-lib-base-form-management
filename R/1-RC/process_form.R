extract_metadata_1RC = function(form) {
  metadata = common_metadata(form$form_metadata)

  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_1RC = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:10]
  colnames(strata) = c("QUARTER", "FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "RETAIN_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[3:nrow(form_data), 11:ncol(form_data)]

  species_codes = unlist(lapply(records[1], trim), use.names = FALSE)

  #colnames(records) = species_codes

  records = records[2:nrow(records), lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = list(
          species_codes = species_codes
        ),
        data = list(
          catches = records
        )
      )
    )
  )
}
