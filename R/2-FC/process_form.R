extract_metadata_2FC = function(form) {
  metadata = common_metadata(form$form_metadata)

  metadata$comment = comments(form$form_metadata, 23)

  return(metadata)
}

extract_strata_and_records_2FC = function(form) {
  form_metadata = form$form_metadata
  form_data     = form$form_data

  strata = form_data[4:nrow(form_data)][, 2:15]
  colnames(strata) = c("GEAR_CODE", "IOTC_MAIN_AREA_CODE",
                       "DATA_TYPE_CODE", "DATA_SOURCE_CODE", "DATA_PROCESSING_CODE",
                       "COVERAGE_TYPE_CODE", "COVERAGE",
                       "BOAT_TYPE_CODE", "MECHANIZATION_TYPE_CODE",
                       "ONBOARD_PRESERVATION_TYPE_CODE", "ONBOARD_PROCESSING_CODE",
                       "SIZE_CLASS_TYPE_CODE", "CLASS_LOW", "CLASS_HIGH")

  strata[, COVERAGE   := round(as.numeric(COVERAGE),   0)]
  strata[, CLASS_LOW  := round(as.numeric(CLASS_LOW),  2)]
  strata[, CLASS_HIGH := round(as.numeric(CLASS_HIGH), 2)]

  records = form_data[4:nrow(form_data), 16]
  records = records[1:nrow(records), lapply(.SD, function(value) { return(round(as.numeric(value), 0)) })]

  return(
    list(
      strata  = strata,
      records = list(
        data = list(
          fishing_crafts = records
        )
      )
    )
  )
}
