#library(iotc.data.common.workflow.interim)
library(data.table)
library(lubridate)
library(openxlsx)
library(stringr)
library(iotc.core.db.data)
library(iotc.data.reference.codelists)

source("./data-raw/LOAD_RAV.R")
source("./data-raw/READ_FISHERY_MAPPINGS.R")
source("./data-raw/READ_EFFORT_MAPPINGS.R")
source("./data-raw/READ_MEASURE_MAPPINGS.R")

source("./R/constants.R")
source("./R/utilities.R")
source("./R/reference_checks.R")
source("./R/field_checks.R")
source("./R/data_checks.R")
source("./R/metadata.R")
source("./R/forms.R")
source("./R/validation_summary_helpers.R")
source("./R/utilities_export.R")
source("./R/utilities_export_IOTDB.R")

debugSource("./R/Message_class.R",               echo = TRUE)
debugSource("./R/IOTC_form_class.R",             echo = TRUE)
debugSource("./R/IOTC_form_RCDI_class.R",        echo = TRUE)
debugSource("./R/IOTC_form_1RC_class.R",         echo = TRUE)
debugSource("./R/IOTC_form_1DI_class.R",         echo = TRUE)
debugSource("./R/IOTC_form_3BU_class.R",         echo = TRUE)
debugSource("./R/IOTC_form_CESF_class.R",        echo = TRUE)
debugSource("./R/IOTC_form_CESF_update_class.R", echo = TRUE)
debugSource("./R/IOTC_form_3CE_class.R",         echo = TRUE)
debugSource("./R/IOTC_form_3CE_update_class.R",  echo = TRUE)
debugSource("./R/IOTC_form_4SF_class.R",         echo = TRUE)
debugSource("./R/IOTC_form_4SF_update_class.R",  echo = TRUE)

FORM_1RC =
  new("IOTCForm1RC",
      path_to_file  = "./test/sample_forms/Form-1RC.xlsx",
      original_name = "Form-1RC.xlsx"
  )

summary = validation_summary(FORM_1RC)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-1RC - summary.csv",
            sep = ",", row.names = FALSE)

out_1RC      = extract_output(FORM_1RC, wide = FALSE)
out_1RC_wide = extract_output(FORM_1RC, wide = TRUE)

FORM_1DI =
  new("IOTCForm1DI",
      path_to_file  = "./test/sample_forms/Form-1DI.xlsx",
      original_name = "Form-1DI.xlsx"
  )

summary = validation_summary(FORM_1DI)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-1DI - summary.csv",
            sep = ",", row.names = FALSE)

out_1DI      = extract_output(FORM_1DI, wide = FALSE)
out_1DI_wide = extract_output(FORM_1DI, wide = TRUE)

FORM_3BU =
  new("IOTCForm3BU",
      path_to_file  = "./test/sample_forms/Form-3BU.xlsx",
      original_name = "Form-3BU.xlsx"
  )

summary = validation_summary(FORM_3BU)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-3BU - summary.csv",
            sep = ",", row.names = FALSE)

out_3BU      = extract_output(FORM_3BU, wide = FALSE)
out_3BU_wide = extract_output(FORM_3BU, wide = TRUE)

FORM_3CE =
  new("IOTCForm3CE",
      path_to_file  = "./test/sample_forms/Form-3CE.xlsx",
      original_name = "Form-3CE.xlsx"
  )

summary = validation_summary(FORM_3CE)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-3CE - summary.csv",
            sep = ",", row.names = FALSE)

out_3CE      = extract_output(FORM_3CE, wide = FALSE)
out_3CE_wide = extract_output(FORM_3CE, wide = TRUE)

FORM_3CE_UPD =
  new("IOTCForm3CEUpdate",
      path_to_file  = "./test/sample_forms/Form-3CE-update.xlsx",
      original_name = "Form-3CE-multiple.xlsx"
  )

summary = validation_summary(FORM_3CE_UPD)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-3CE-update - summary.csv",
            sep = ",", row.names = FALSE)

out_3CE_UPD      = extract_output(FORM_3CE_UPD, wide = FALSE)
out_3CE_UPD_wide = extract_output(FORM_3CE_UPD, wide = TRUE)

FORM_4SF =
  new("IOTCForm4SF",
      path_to_file  = "./test/sample_forms/Form-4SF.xlsx",
      original_name = "Form-4SF.xlsx"
  )

summary = validation_summary(FORM_4SF)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-4SF - summary.csv",
            sep = ",", row.names = FALSE)

out_4SF      = extract_output(FORM_4SF, wide = FALSE)
out_4SF_wide = extract_output(FORM_4SF, wide = TRUE)

FORM_4SF_UPD =
  new("IOTCForm4SFUpdate",
      path_to_file  = "./test/sample_forms/Form-4SF-update.xlsx",
      original_name = "Form-4SF-update.xlsx"
  )

summary = validation_summary(FORM_4SF_UPD)

write.table(summary$validation_messages,
            file = "./test/sample_forms/Form-4SF-update - summary.csv",
            sep = ",", row.names = FALSE)

out_4SF_UPD      = extract_output(FORM_4SF_UPD, wide = FALSE)
out_4SF_UPD_wide = extract_output(FORM_4SF_UPD, wide = TRUE)
