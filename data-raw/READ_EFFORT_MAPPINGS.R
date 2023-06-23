EFFORT_MAPPINGS = read.csv("./data-raw/EFFORT_UNITS_CONVERSION.csv", na.strings = "")

usethis::use_data(EFFORT_MAPPINGS, overwrite = TRUE)
