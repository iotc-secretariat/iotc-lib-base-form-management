verify_form_1DI = function(form) {
  form_details = extract_metadata_1DI(form)$form_details

  form_type    = form_details$type
  form_version = form_details$version

  if(check_mandatory(form_type, "Form type") != "1-DI")
    stop(paste0("Please provide a valid form 1-DI (current form type: ", form_type, " - required: 1-DI)"))

  if(check_mandatory(form_version, "Form version") != "1.0.0")
    stop(paste0("Please provide a valid form 1-DI (current form version: ", form_version, " - required: 1.0.0)"))
}

extract_metadata_1DI = function(form) {
  metadata = common_metadata(form$form_metadata)

  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_1DI = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[7:nrow(form_data)][, 2:10]
  colnames(strata) = c("QUARTER", "FISHERY_CODE", "IOTC_MAIN_AREA_CODE", "DISCARD_REASON_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE")

  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]

  records = form_data[3:nrow(form_data), 12:ncol(form_data)]

  species_codes    = unlist(lapply(records[1], trim), use.names = FALSE)
  condition_codes  = unlist(lapply(records[2], trim), use.names = FALSE)
  raising_codes    = unlist(lapply(records[3], trim), use.names = FALSE)
  catch_unit_codes = unlist(lapply(records[4], trim), use.names = FALSE)

  #colnames(records) = species_codes

  records = records[5:nrow(records), lapply(.SD, function(value) { return(round(as.numeric(value), 2)) })]

  return(
    list(
      strata  = strata,
      records = list(
        codes = data.table(
          species     = species_codes,
          conditions  = condition_codes,
          raisings    = raising_codes,
          catch_units = raising_codes
        ),
        data = list(
          discards = records
        )
      )
    )
  )
}
