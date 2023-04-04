verify_form_3CE_multiple = function(form) {
  form_details = extract_metadata_1DI(form)$form_details

  form_type    = form_details$type
  form_version = form_details$version

  if(check_mandatory(form_type, "Form type") != "3-CE-multiple")
    stop(paste0("Please provide a valid form 3-CE-multiple (current form type: ", form_type, " - required: 3-CE-multiple)"))

  if(check_mandatory(form_version, "Form version") != "1.0.0")
    stop(paste0("Please provide a valid form 3-CE-multiple (current form version: ", form_version, " - required: 1.0.0)"))
}

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
          species     = species_codes,
          catch_units = catch_unit_codes
        ),
        data = list(
          efforts = efforts,
          catches = catches
        )
      )
    )
  )
}
