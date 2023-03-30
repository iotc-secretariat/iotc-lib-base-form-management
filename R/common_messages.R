### COMMON MESSAGE MANAGEMENT FUNCTIONS

initialize_messages = function() {
  return(
    list(
      INFO  = data.table(SHEET = character(), ROW = integer(), COLUMN = integer(), TEXT = character()),
      WARN  = data.table(SHEET = character(), ROW = integer(), COLUMN = integer(), TEXT = character()),
      ERROR = data.table(SHEET = character(), ROW = integer(), COLUMN = integer(), TEXT = character()),
      FATAL = data.table(SHEET = character(), ROW = integer(), COLUMN = integer(), TEXT = character())
    )
  )
}

create_message = function(text, sheet = METADATA_SHEET, row = NA, column = NA) {
  return(
    data.table(
      SHEET  = sheet,
      ROW    = row,
      COLUMN = column,
      TEXT   = text
    )
  )
}

add_info = function(messages, message_info) {
  messages$INFO = rbind(messages$INFO, message_info)

  return(messages)
}

add_warning = function(messages, message_warn) {
  messages$WARN = rbind(messages$WARN, message_warn)

  return(messages)
}

add_error = function(messages, message_error) {
  messages$ERROR = rbind(messages$ERROR, message_error)

  return(messages)
}

add_fatal = function(messages, message_fatal) {
  messages$FATAL = rbind(messages$FATAL, message_fatal)

  return(messages)
}
