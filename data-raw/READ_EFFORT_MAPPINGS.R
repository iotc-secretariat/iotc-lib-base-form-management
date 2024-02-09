EFFORT_MAPPINGS = read.csv("./data-raw/mappings/IOTCStatistics_IOTDB/EFFORT_UNITS_MAPPINGS.csv", na.strings = "")

usethis::use_data(EFFORT_MAPPINGS, overwrite = TRUE)
