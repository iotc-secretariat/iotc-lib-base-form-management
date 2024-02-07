EFFORT_MAPPINGS = read.csv("./mappings/IOTC_master_IOTDB/EFFORT_UNITS_MAPPINGS.csv", na.strings = "")

usethis::use_data(EFFORT_MAPPINGS, overwrite = TRUE)
