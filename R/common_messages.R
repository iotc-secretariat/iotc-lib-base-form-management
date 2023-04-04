### COMMON MESSAGE MANAGEMENT FUNCTIONS

initialize_messages = function() {
  return(
    list(
      INFO  = data.table(sheet = character(), row = integer(), column = integer(), message = character()),
      WARN  = data.table(sheet = character(), row = integer(), column = integer(), message = character()),
      ERROR = data.table(sheet = character(), row = integer(), column = integer(), message = character()),
      FATAL = data.table(sheet = character(), row = integer(), column = integer(), message = character())
    )
  )
}

create_message = function(message, sheet = NA, row = NA, column = NA) {
  return(
    data.table(
      sheet   = sheet,
      row     = row,
      column  = column,
      message = message
    )
  )
}

add_info = function(messages, message) {
  messages$INFO = rbind(messages$INFO, message)

  return(messages)
}

add_warning = function(messages, message) {
  messages$WARN = rbind(messages$WARN, message)

  return(messages)
}

add_error = function(messages, message) {
  messages$ERROR = rbind(messages$ERROR, message)

  return(messages)
}

add_fatal = function(messages, message) {
  messages$FATAL = rbind(messages$FATAL, message)

  return(messages)
}
