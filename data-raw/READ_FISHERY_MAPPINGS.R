FISHERY_MAPPINGS = read.csv("./mappings/IOTC_master_IOTDB/FISHERIES_TO_GEARS_MAPPINGS.csv", na.strings = "")

usethis::use_data(FISHERY_MAPPINGS, overwrite = TRUE)
