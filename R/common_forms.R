### COMMON FORMS FUNCTIONS

read_form = function(filename) {
  form_meta = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = filename, sheet = METADATA_SHEET, skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(cond) {
    stop(paste0("Unable to read metadata from form ", filename, ": ", cond))
  })

  form_data = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = filename, sheet = DATA_SHEET,     skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(cond) {
    stop(paste0("Unable to read data from form ", filename, ": ", cond))
  })

  return(
    list(
      form_metadata = form_meta,
      form_data     = form_data
    )
  )
}
