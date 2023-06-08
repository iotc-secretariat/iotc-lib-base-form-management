### COMMON FORMS FUNCTIONS

#' @export
read_form = function(path_to_file, original_name = NA) {
  display_filename = basename(ifelse(is.na(original_name), path_to_file, original_name))

  form_meta = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = path_to_file, sheet = METADATA_SHEET, skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(e) {
    stop(paste0("File format error: unable to read metadata from ", display_filename, ": ", e$message))
  })

  form_data = tryCatch({
    as.data.table(
      read.xlsx(xlsxFile = path_to_file, sheet = DATA_SHEET, skipEmptyRows = FALSE, skipEmptyCols = FALSE, detectDates = TRUE)
    )
  },
  error = function(e) {
    stop(paste0("File format error: unable to read data from ", display_filename, ": ", e$message))
  })

  return(
    list(
      form_metadata = form_meta,
      form_data     = form_data
    )
  )
}
