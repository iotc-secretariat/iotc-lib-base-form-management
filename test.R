#library(iotc.data.common.workflow.legacy)
library(memoise)
library(data.table)
library(openxlsx)
library(stringr)
library(iotc.core.db.data)
library(iotc.data.reference.codelists)

source("./R/constants.R")
source("./R/utilities.R")
source("./R/reference_checks.R")
source("./R/field_checks.R")
source("./R/data_checks.R")
source("./R/metadata.R")
source("./R/forms.R")

debugSource("./R/Message_class.R",                 echo = TRUE)
debugSource("./R/IOTC_form_class.R",               echo = TRUE)
debugSource("./R/IOTC_form_RCDI_class.R",          echo = TRUE)
debugSource("./R/IOTC_form_1RC_class.R",           echo = TRUE)
debugSource("./R/IOTC_form_1DI_class.R",           echo = TRUE)
debugSource("./R/IOTC_form_3BU_class.R",           echo = TRUE)
debugSource("./R/IOTC_form_CESF_class.R",          echo = TRUE)
debugSource("./R/IOTC_form_CESF_multiple_class.R", echo = TRUE)
debugSource("./R/IOTC_form_3CE_class.R",           echo = TRUE)
debugSource("./R/IOTC_form_3CE_multiple_class.R",  echo = TRUE)
debugSource("./R/IOTC_form_4SF_class.R",           echo = TRUE)
debugSource("./R/IOTC_form_4SF_multiple_class.R",  echo = TRUE)

FORM_1RC =
  new("IOTCForm1RC",
      path_to_file  = "./test_forms/Form-1RC - legacy.xlsx",
      original_name = "Form-1RC.xlsx"
  )

#summary = validation_summary(FORM_1RC)

FORM_1DI =
  new("IOTCForm1DI",
      path_to_file  = "./test_forms/Form-1DI - legacy.xlsx",
      original_name = "Form-1DI.xlsx"
  )

#summary = validation_summary(FORM_1DI)

FORM_3BU =
  new("IOTCForm3BU",
      path_to_file  = "./test_forms/Form-3BU - legacy.xlsx",
      original_name = "Form-3BU.xlsx"
  )

summary = validation_summary(FORM_3BU)

FORM_3CE =
  new("IOTCForm3CE",
      path_to_file  = "./test_forms/Form-3CE - legacy.xlsx",
      original_name = "Form-3CE.xlsx"
  )

#summary = validation_summary(FORM_3CE)

FORM_3CE_MUL =
  new("IOTCForm3CEMultiple",
      path_to_file  = "./test_formsForm-3CE-multiple - legacy.xlsx",
      original_name = "Form-3CE-multiple.xlsx"
  )

#summary = validation_summary(FORM_3CE_MUL)

FORM_4SF =
  new("IOTCForm4SF",
      path_to_file  = "./test_forms/Form-4SF - legacy.xlsx",
      original_name = "Form-4SF.xlsx"
  )

#summary = validation_summary(FORM_4SF)

FORM_4SF_MUL =
  new("IOTCForm4SFMultiple",
      path_to_file  = "./test_forms/Form-4SF-multiple - legacy.xlsx",
      original_name = "Form-4SF-multiple.xlsx"
  )

#summary = validation_summary(FORM_4SF_MUL)
