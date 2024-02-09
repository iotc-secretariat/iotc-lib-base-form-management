FISHERY_MAPPINGS = read.csv("./data-raw/mappings/IOTCStatistics_IOTDB/FISHERIES_TO_GEARS_MAPPINGS.csv", na.strings = "")

usethis::use_data(FISHERY_MAPPINGS, overwrite = TRUE)
