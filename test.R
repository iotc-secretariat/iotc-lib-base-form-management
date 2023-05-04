library(iotc.data.common.workflow)
#library(data.table)
#library(openxlsx)
#library(stringr)
#library(iotc.core.utils.misc)
#library(iotc.data.reference.codelists)

source("R/constants.R")
source("R/utilities.R")
source("R/reference_checks.R")
source("R/field_checks.R")
source("R/data_checks.R")
source("R/metadata.R")
source("R/forms.R")

FORM_1RC =
  new("IOTCForm1RC",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-1RC_test.xlsx",
      original_name = "Form-1RC.xlsx"
  )

validation_summary(FORM_1RC)

FORM_1DI =
  new("IOTCForm1DI",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-1DI_test.xlsx",
      original_name = "Form-1DI.xlsx"
  )

validation_summary(FORM_1DI)


