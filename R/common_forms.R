### COMMON FORMS FUNCTIONS

read_form = function(file_name, original_name = NA) {
  display_filename = basename(ifelse(is.na(original_name), file_name, original_name))

  form_meta = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = file_name, sheet = METADATA_SHEET, skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(e) {
    stop(paste0("Unable to read metadata from ", display_filename, ": ", e$message))
  })

  form_data = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = file_name, sheet = DATA_SHEET, skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(e) {
    stop(paste0("Unable to read data from ", display_filename, ": ", e$message))
  })

  return(
    list(
      form_metadata = form_meta,
      form_data     = form_data
    )
  )
}
