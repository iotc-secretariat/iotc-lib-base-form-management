### COMMON METADATA CHECKS

validate_common_metadata = function(form_metadata) {
  submission_information = submission_information(form_metadata)

  mandatory(submission_information$focal_point$full_name,        "Focal point name")
  mandatory(submission_information$focal_point$e_mail,           "Focal point e-mail")

  mandatory(submission_information$organization$full_name,       "Organization name")
  mandatory(submission_information$organization$e_mail,          "Organization e-mail")

  mandatory(submission_information$reference_dates$finalization, "Finalization date")
  mandatory(submission_information$reference_dates$submission,   "Submission date")

  general_information = general_information(form_metadata)

  validate_year(mandatory(general_information$reporting_year,    "Reporting year"))

  validate_fleet(
    mandatory(general_information$reporting_entity,              "Reporting entity"),
    mandatory(general_information$flag_country,                  "Flag country")
  )

  return(form_metadata)
}
