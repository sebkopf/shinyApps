#' @param post text after the input
#' @param type the kind of input, e.g. number or text
#' @param class what style it has 
generate_input_row <- function (id, label, post = NULL, type = "number", class = "input-mini", width = "60px", ...) {
  div(style="display:inline-block",
      tags$label(label, `for` = id), 
      tags$input(id = id, type = type, ..., class = class, style=paste0("width: ", width, ";")),
      if(!is.null(post)) tags$label(post))
}


# label strengths selection
label_picker <- function(vols, concs, strengths) {
  r <- list(
    
  )
  for (i in 1:length(strengths)) {
    r <- c(r, list(
      fluidRow(
        column(1, span(paste0("s#", i), title = paste0("Isotope spike #", i))),
        column(3, numericInput(paste0("label", i, "_vol"), "", vols[i], min = 0)),
        column(3, offset = 1, numericInput(paste0("label", i, "_conc"), "", concs[i], min = 0)),
        column(3, offset = 1, sliderInput(paste0("label", i), "", min = 0, max = 100, step = 1, value = strengths[i], post = "%"))
      ),
      htmlOutput(paste0("label", i, "_error")),
      tags$style(type="text/css", HTML(paste0("#label", i, "_error {color: #f50000;}"))),
      htmlOutput(paste0("label", i, "_msg")),
      tags$style(type="text/css", HTML(paste0("#label", i, "_msg {color: #00AF64;}")))
    ))
  }
  r <- c(r, list(
    checkboxGroupInput(inputId="show", label="Show labeling strengths (spike #):", choices=1:length(strengths), selected=1:length(strengths)),
    tags$style(type="text/css", HTML("#show>*{float: left; margin-right: 15px; height: 20px;} #show {height: 20px;}"))))
  r
}
