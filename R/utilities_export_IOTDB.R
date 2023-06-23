convert_area = function(code) {
  return(
    fifelse(code == "IOTC_WEST", "F51",
            fifelse(code == "IOTC_EAST", "F57",
                    code))
  )
}

convert_catch_unit = function(code) {
  return(
    fifelse(code == "MT", "T", code)
  )
}

convert_1RC = function(filename, source_code = "LO", quality_code = "GOOD") {
  form = new("IOTCForm1RC", path_to_file = filename)

  output = extract_output(form, FALSE)

  output =
    output[, `:=`(START_DATE = quarter_to_date_start(YEAR, QUARTER),
                  END_DATE   = quarter_to_date_end  (YEAR, QUARTER))]

  return(
    output[, .(Country = FLAG_COUNTRY_CODE,
               ReportingCountry = REPORTING_ENTITY_CODE,
               TimeIntervalStart = START_DATE,
               TimeIntervalEnd = END_DATE,
               Grid = convert_area(IOTC_MAIN_AREA_CODE),
               Gear = MAIN_GEAR_CODE,
               Species = SPECIES_CODE,
               Catch = CATCH,
               CatchUnit = convert_catch_unit(CATCH_UNIT_CODE),
               Source = source_code,
               QualityCode = quality_code,
               EComments = NA,
               FComments = NA)]
  )
}

convert_3CE = function(filename, source_code = "LO", quality_code = "GOOD") {
  form = new("IOTCForm3CE", path_to_file = filename)

  return(
    do_convert_3CE(
      extract_output(form, FALSE),
      source_code,
      quality_code
    )
  )
}

convert_3CE_multiple = function(filename, source_code = "LO", quality_code = "GOOD") {
  form = new("IOTCForm3CEMultiple", path_to_file = filename)

  return(
    do_convert_3CE(
      extract_output(form, FALSE),
      source_code,
      quality_code
    )
  )
}

do_convert_3CE = function(output, source_code, quality_code) {
  output =
    output[, `:=`(START_DATE = month_to_date_start(YEAR, MONTH),
                  END_DATE   = month_to_date_end  (YEAR, MONTH))]

  output =
    output[, .(CO = FLAG_COUNTRY_CODE,
               GEAR = MAIN_GEAR_CODE,
               YEAR = YEAR,
               MONTH = month(ymd(START_DATE)),
               SCHOOLTYPE = SCHOOL_TYPE_CODE,
               SIZE = str_sub(GRID_CODE, 1, 1),
               Q    = str_sub(GRID_CODE, 2, 2),
               LAT  = as.integer(str_sub(GRID_CODE, 3, 4)),
               LON  = as.integer(str_sub(GRID_CODE, 5, 7)),
               EUNIT = PRIMARY_EFFORT_CODE,
               CUNIT = convert_catch_unit(CATCH_UNIT_CODE),
               EFFORT = PRIMARY_EFFORT,
               SPECIES_CODE,
               CATCH)]

  output = merge(output, EFFORT_MAPPINGS, by.x = "EUNIT", by.y = "CODE", all.x = TRUE)

  output[, TOTAL := sum(CATCH, na.rm = TRUE), by = .(CO, GEAR, YEAR, MONTH, SCHOOLTYPE, SIZE, Q, LAT, LON, EUNIT, EFFORT_CODE, CUNIT, EFFORT)]
  output[!is.na(EFFORT_CODE), EUNIT := EFFORT_CODE]

  return(
    dcast.data.table(
      data = output,
      formula = CO + GEAR + YEAR + MONTH + SCHOOLTYPE + SIZE + Q + LAT + LON + EUNIT + CUNIT + EFFORT + TOTAL ~ SPECIES_CODE,
      fun.aggregate = function(v) { return(sum(v, na.rm = TRUE)) },
      value.var = "CATCH"
    )
  )
}

convert_4SF = function(filename, source_code = "LO", quality_code = "GOOD") {
  form = new("IOTCForm4SF", path_to_file = filename)

  return(
    do_convert_4SF(
      extract_output(form, FALSE),
      source_code,
      quality_code
    )
  )
}

convert_4SF_multiple = function(filename, source_code = "LO", quality_code = "GOOD") {
  form = new("IOTCForm4SFMultiple", path_to_file = filename)

  return(
    do_convert_4SF(
      extract_output(form, FALSE),
      source_code,
      quality_code
    )
  )
}

do_convert_4SF = function(output, source_code, quality_code) {
  output =
    output[, `:=`(START_DATE = month_to_date_start(YEAR, MONTH),
                  END_DATE   = month_to_date_end  (YEAR, MONTH))]

  return(
    output[, .(Country = FLAG_COUNTRY_CODE,
               ReportingCountry = REPORTING_ENTITY_CODE,
               TimeIntervalStart = START_DATE,
               TimeIntervalEnd = END_DATE,
               Grid = GRID_CODE,
               Gear = MAIN_GEAR_CODE,
               Species = SPECIES_CODE,
               SchoolType = SCHOOL_TYPE_CODE,
               Source = source_code,
               MeasType = MEASURE_CODE,
               RaisingCode = DATA_RAISING_CODE,
               #SampleSize = round(as.numeric(NUM_SAMPLES), 2),
               SampleSize = round(as.numeric(NUM_SAMPLES_STRATA), 2),
               QualityCode = quality_code,
               ClassLow = SIZE_CLASS_LOW,
               ClassHigh = SIZE_CLASS_HIGH,
               FishCount = round(as.numeric(NUM_FISH), 2),
               SexCode = SEX_CODE)]
  )
}
