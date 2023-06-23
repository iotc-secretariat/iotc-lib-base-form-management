FISHERY_MAPPINGS = read.csv("./data-raw/FISHERIES_TO_GEARS.csv", na.strings = "")

usethis::use_data(FISHERY_MAPPINGS, overwrite = TRUE)
