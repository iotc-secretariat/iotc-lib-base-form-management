MEASURE_MAPPINGS = read.csv("./mappings/IOTC_master_IOTDB/MEASURE_TYPES_MAPPINGS.csv", na.strings = "")

usethis::use_data(MEASURE_MAPPINGS, overwrite = TRUE)
