
# label strengths selection
label_picker <- function(vols, concs, strengths) {
  r <- list(
    fluidRow(
      # adds up to 12 with the offsets
      column(3, offset = 1, strong("Volume")),
      column(3, offset = 1, strong("Concentration")),
      column(3, offset = 1, strong(htmlOutput("rare_iso_header")))),
    fluidRow(
      column(1, span("init", title = "The initial isotopic composition of the sample.")),
      column(3, numericInput("label.ref_vol", "", 1000, min = 0)),
      column(3, offset = 1, numericInput("label.ref_conc", "", 1, min = 0)),
      column(3, offset = 1, "natural")
    ),
    htmlOutput("nat"))
  for (i in 1:length(strengths)) {
    r <- c(r, list(
      fluidRow(
        column(1, span(paste0("s#", i), title = paste0("Isotope spike #", i))),
        column(3, numericInput(paste0("label", i, "_vol"), "", vols[i], min = 0)),
        column(3, offset = 1, numericInput(paste0("label", i, "_conc"), "", concs[i], min = 0)),
        column(3, offset = 1, sliderInput(paste0("label", i), "", min = 0, max = 1, step = 0.01, value = strengths[i], post = "%"))
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
