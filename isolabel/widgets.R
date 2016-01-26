#' @param post text after the input
#' @param type the kind of input, e.g. number or text
#' @param class what style it has 
generate_input_row <- function (id, label, post = NULL, type = "number", class = "input-mini", width = "60px", ...) {
  div(style="display:inline-block",
      tags$label(label, `for` = id), 
      tags$input(id = id, type = type, ..., class = class, style=paste0("width: ", width, ";")),
      if(!is.null(post)) tags$label(post))
}

