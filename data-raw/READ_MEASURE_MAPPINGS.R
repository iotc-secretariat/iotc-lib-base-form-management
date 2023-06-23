MEASURE_MAPPINGS = read.csv("./data-raw/MEASURE_TYPES_CONVERSION.csv", na.strings = "")

usethis::use_data(MEASURE_MAPPINGS, overwrite = TRUE)
