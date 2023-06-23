quarter_to_date_start = function(year, quarter) {
  return(
    as.Date(
      paste0(year,
             fifelse(quarter == 0, "-01-01",
                     paste0("-", str_sub(paste0("0", quarter * 3 - 2), -2), "-01")
             )
      )
    )
  )
}

quarter_to_date_end = function(year, quarter) {
  return(
    as.Date(
      paste0(year,
             fifelse(quarter == 0, "-12-31",
                     paste0('-', str_sub(paste0("0", quarter * 3), -2), '-',
                                 str_sub(paste0("0", ifelse(quarter %in% 2:3, 30, 31)), -2))
             )
      )
    )
  )
}

month_to_date_start = function(year, month) {
  return(
    as.Date(paste0(year, '-', month, '-01'))
  )
}

month_to_date_end = function(year, month) {
  return(
    ceiling_date(month_to_date_start(year, month), 'month') - days(1) # Accounts for leap days in February
  )
}

