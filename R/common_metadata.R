### COMMON FUNCTIONS TO EXTRACT / READ DATA FROM IOTC (NEW) FORMS

form_details = function(metadata_sheet) {
  return(
    list(
      type    = trim(as.character(metadata_sheet[2, 4])),
      version = trim(as.character(metadata_sheet[2, 7]))
    )
  )
}

focal_point_details = function(metadata_sheet) {
  return(
    list(
      full_name = trim(as.character(metadata_sheet[7, 4])),
      e_mail    = trim(as.character(metadata_sheet[8, 4]))
    )
  )
}

organization_details = function(metadata_sheet) {
  return(
    list(
      full_name = trim(as.character(metadata_sheet[7, 7])),
      e_mail    = trim(as.character(metadata_sheet[8, 7]))
    )
  )
}

reference_dates = function(metadata_sheet) {
  return(
    list(
      finalization = as.Date(trim(as.character(metadata_sheet[10, 4]))),
      submission   = as.Date(trim(as.character(metadata_sheet[11, 4])))
    )
  )
}

submission_information = function(metadata_sheet) {
  return(
    list(
      focal_point  = focal_point_details(metadata_sheet),
      organization = organization_details(metadata_sheet),
      reference_dates = reference_dates(metadata_sheet)
    )
  )
}

general_information = function(metadata_sheet) {
  return(
    list(
      reporting_year   = as.integer(trim(as.character(metadata_sheet[16, 4]))),
      reporting_entity = trim(as.character(metadata_sheet[17, 4])),
      flag_country     = trim(as.character(metadata_sheet[18, 4]))
    )
  )
}

common_metadata = function(metadata_sheet) {
  return(
    list(
      form_details           = form_details(metadata_sheet),
      submission_information = submission_information(metadata_sheet),
      general_information    = general_information(metadata_sheet)
    )
  )
}

comments = function(metadata_sheet, row) {
  #stop("Comments have to be extracted on a form-by-form basis, as the location of the cell changes")

  if(nrow(metadata_sheet) < row)
    return(NA)

  return(trim(as.character(metadata_sheet[row, 3])))
}
