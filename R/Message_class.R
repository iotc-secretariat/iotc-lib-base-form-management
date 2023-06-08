#' @export Message
Message = setClass("Message",   representation(level  = "character",
                                               source = "character",
                                               row    = "numeric",
                                               column = "character",
                                               text   = "character"),
                                prototype     (level  = "INFO",
                                               source = "data",
                                               row    = NA_integer_,
                                               column = NA_character_))

setGeneric("as_list", function(message) {
  standardGeneric("as_list")
})

setMethod("as_list", "Message", function(message) {
  return(
    list(
      level  = message@level,
      source = message@source,
      row    = message@row,
      column = message@column,
      text   = message@text
    )
  )
})

setGeneric("as_data_table", function(message) {
  standardGeneric("as_data_table")
})

setMethod("as_data_table", "Message", function(message) {
  return(
    data.table(
      LEVEL  = message@level,
      SOURCE = message@source,
      ROW    = message@row,
      COLUMN = message@column,
      TEXT   = message@text
    )
  )
})

#' @export MessageList
MessageList = setClass("MessageList", representation(messages = "data.table"),
                                      prototype     (messages = data.table(LEVEL  = character(),
                                                                           SOURCE = character(),
                                                                           ROW    = numeric(),
                                                                           COLUMN = character(),
                                                                           TEXT   = character())))

setGeneric("add", function(messageList, message) {
  standardGeneric("add")
})

#' @export
setMethod("add", list("MessageList", "Message"),
          function(messageList, message) {
            messageList@messages =
              rbind(
                messageList@messages,
                data.table(LEVEL  = message@level,
                           SOURCE = message@source,
                           ROW    = message@row,
                           COLUMN = message@column,
                           TEXT   = message@text)
              )

            return(messageList)
          }
)

setGeneric("messages_by_level", function(messageList, level) {
  standardGeneric("messages_by_level")
})

setMethod("messages_by_level", list("MessageList", "character"), function(messageList, level) {
  return(
    messageList@messages[LEVEL == level]
  )
})
