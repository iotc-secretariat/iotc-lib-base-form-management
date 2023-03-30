UI_glasspane = function() {
  return(
    conditionalPanel(
      condition="$('html').hasClass('shiny-busy')",
      tags$div(id="glasspane",
               tags$div(class="loading", "Please wait...")
      )
    )
  )
}

label = function(text, idFor = NA, class = "col-sm-4 control-label", style = NA) {
  return(
    HTML(
      paste0(
        "<label",
        ifelse(is.na(idFor), "", paste0(" for=\"", idFor, "\"")),
        ifelse(is.na(class), "", paste0(" class=\"", class, "\"")),
        ifelse(is.na(style), "", paste0(" style=\"", style, "\"")),
        ">",
        text,
        "</label>"
      )
    )
  )
}

textInput = function(value, enabled = TRUE, readonly = TRUE, id = NA, class = "form-control", style = NA) {
  return(
    div(class = "col-sm-8",
        HTML(
          paste0(
            "<input type = \"text\"",
            ifelse(!enabled, " disabled", ""),
            " readonly=\"", ifelse(readonly, "true", "false"), "\"",
            ifelse(is.na(id),    "", paste0(" id=\"", id, "\"")),
            ifelse(is.na(class), "", paste0(" class=\"", class, "\"")),
            ifelse(is.na(style), "", paste0(" style=\"", style, "\"")),
            ifelse(is.na(value), "", paste0(" value=\"", value, "\"")),
            "/>"
          )
        )
    )
  )
}

badge = function(value) {
  return(span(class = "badge fixed", value))
}

summary_ = function(label, success, additional_data = NA, failure_icon = "remove", failure_class = "danger") {
  if(!is_available(additional_data)) {
    extra = span("")
  } else {
    extra = lapply(additional_data, badge)
  }

  return(
    p(class = paste0("clearfix bg-", ifelse(!is.na(success) & success, "success", failure_class)),
      style = "padding: .5em;",
      strong(
        icon(lib = "glyphicon", ifelse(success, "ok", failure_icon)),
        label
      ),
      span(class="pull-right", extra)
    )
  )
}

summary = function(label, success = TRUE, additional_data = NA) {
  return(summary_(label, success, additional_data))
}

warn    = function(label, success = TRUE, additional_data = NA) {
  return(summary_(label, success, additional_data, failure_icon = "warning-sign", failure_class = "warning"))
}

success = function(label, additional_data = NA) {
  return(summary(label, success = TRUE, additional_data))
}

failure = function(label, additional_data = NA) {
  return(summary(label, success = FALSE, additional_data))
}

textAreaInput = function(value, rows = 6, enabled = TRUE, readonly = TRUE, id = NA, class = "form-control", style = NA) {
  return(
    div(class = "col-sm-8",
        HTML(
          paste0(
            "<textarea ",
            " rows=", rows,
            ifelse(!enabled, " disabled", ""),
            " readonly=\"", ifelse(readonly, "true", "false"), "\"",
            ifelse(is.na(id),    "", paste0(" id=\"", id, "\"")),
            ifelse(is.na(class), "", paste0(" class=\"", class, "\"")),
            ifelse(is.na(style), "", paste0(" style=\"", style, "\"")),
            ifelse(is.na(value), "", paste0(" value=\"", value, "\"")),
            ">",
            value ,
            "</textarea>"
          )
        )
    )
  )
}

format_value = function(value) {
  return(format(round(value, 2), big.mark = ",", small.mark = "."))
}
