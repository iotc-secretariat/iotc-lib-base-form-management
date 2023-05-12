#library(iotc.data.common.workflow.legacy)
library(data.table)
library(openxlsx)
library(stringr)
library(iotc.core.utils.misc)
library(iotc.data.reference.codelists)

source("R/constants.R")
source("R/utilities.R")
source("R/reference_checks.R")
source("R/field_checks.R")
source("R/data_checks.R")
source("R/metadata.R")
source("R/forms.R")

debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/Message_class.R",        echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_class.R",      echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_RCDI_class.R", echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_1RC_class.R",  echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_1DI_class.R",  echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_CESF_class.R", echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_3CE_class.R",  echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_4SF_class.R",  echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_CESF_multiple_class.R", echo = TRUE)
debugSource("C:/dev/git/bitbucket_workspaces/IOTC-ws/R libs/workflow/R/IOTC_form_3CE_multiple_class.R",  echo = TRUE)

FORM_3CE_MUL =
  new("IOTCForm3CEMultiple",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-3CE-multiple - legacy.xlsx",
      original_name = "Form-3CE-multiple.xlsx"
  )

validation_summary(FORM_3CE_MUL)

FORM_4SF =
  new("IOTCForm4SF",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-4SF - legacy.xlsx",
      original_name = "Form-4SF.xlsx"
  )

validation_summary(FORM_4SF)

FORM_3CE =
  new("IOTCForm3CE",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-3CE - legacy OKK.xlsx",
      original_name = "Form-3CE.xlsx"
  )

validation_summary(FORM_3CE)

FORM_1RC =
  new("IOTCForm1RC",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-1RC - legacy.xlsx",
      original_name = "Form-1RC - legacy.xlsx"
  )

validation_summary(FORM_1RC)

FORM_1DI =
  new("IOTCForm1DI",
      path_to_file  = "C:\\dev\\git\\bitbucket_workspaces\\IOTC-ws\\R libs\\workflow - Copy\\Form-1DI - legacy.xlsx",
      original_name = "Form-1DI.xlsx"
  )

validation_summary(FORM_1DI)


META = read.xlsx("forms/Form-3CE.xlsx", sheet = "Metadata", skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
