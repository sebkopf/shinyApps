library(shiny)
library(shinyIncubator)

# doubling time picking
dblt_picker <- function(i, value = 1, unit = c("minute", "hour", "day", "week", "month", "year")) {
    unit <- match.arg(unit)
    units <- eval(formals(dblt_picker)$unit, list())
    fluidRow(
        column(3, numericInput(paste0("dblt", i), "", value, min = 1, step = 1)),
        column(6, offset = 1, selectInput(paste0("dblt", i, "_units"), "", choices = units, selected = unit))
    )
}

# label strengths selection
label_picker <- function(vols, concs, strengths) {
    r <- list(
        fluidRow(
            column(3, offset = 1, strong("Volume")),
            column(3, offset = 1, strong("Conc.")),
            column(3, offset = 1, strong("Strength"))),
        fluidRow(
            column(1, "ref"),
            column(3, numericInput("label.ref_vol", "", 1000, min = 0)),
            column(3, offset = 1, numericInput("label.ref_conc", "", 1, min = 0)),
            column(3, offset = 1, "natural")
        ),
        htmlOutput("nat"))
    for (i in 1:length(strengths)) {
        r <- c(r, list(
            fluidRow(
                column(1, i),
                column(3, numericInput(paste0("label", i, "_vol"), "", vols[i], min = 0)),
                column(3, offset = 1, numericInput(paste0("label", i, "_conc"), "", concs[i], min = 0)),
                column(3, offset = 1, sliderInput(paste0("label", i), "", min = 0, max = 1, step = 0.01, value = strengths[i], format="#.#%"))
            ),
            htmlOutput(paste0("label", i, "_error")),
            tags$style(type="text/css", HTML(paste0("#label", i, "_error {color: #f50000;}"))),
            htmlOutput(paste0("label", i, "_msg")),
            tags$style(type="text/css", HTML(paste0("#label", i, "_msg {color: #00AF64;}")))
        ))
    }
    r <- c(r, list(
        checkboxGroupInput(inputId="show", label="Show labeling strengths:", choices=1:length(strengths), selected=1:length(strengths)),
        tags$style(type="text/css", HTML("#show>*{float: left; margin-right: 15px; height: 20px;} #show {height: 20px;}"))))
    r
}

# Define UI that plots the isotope label enrichment 
shinyUI(
    fluidPage(
        
        # Application headers
        titlePanel("Isotope labeling calculator"),
        "This app is intended for use in ", em("estimating"), " incubation times and isotopic enrichments in isotope labeling experiments.",
        fluidRow(
            "The source code is available on ",
            a(href="https://www.github.com/sebkopf/shinyApps#isolabel", "GitHub"),
            ". Please use the repository's",
            a(href="https://github.com/sebkopf/shinyApps/issues", "Issue Tracker"),
            "for any feedback, suggestions and bug reports."
        ),
        
        hr(),
        
        sidebarLayout(
            
            # side bar panel
            sidebarPanel(
                h4("Isotope label"),
                selectInput("ref", "", 
                            choices = c("2H", "13C", "15N", "18O", "34S")),
                
#                h5("Label strengths"),
                label_picker(vols = c(1, 5, 10), concs = c(1, 1, 1), strengths = c(0.5, 0.5, 0.99)),
                
                h4("Target enrichment"),
                radioButtons(
                    "targetType", "",
                    c(`Relative enrichment [in permil]` = "permil",
                      `Total abundance [% light isotope]` = "frac"), selected="permil"),
                
                conditionalPanel(
                    condition = "input.targetType == 'permil'",
                    numericInput("intensity_permil", "", 500, min = 0, step = 100)
                ),
                
                conditionalPanel(
                    condition = "input.targetType == 'frac'",
                    sliderInput("intensity_F", "", 
                                min = 0, max = 1, step = 0.001, value = 0.05, 
                                format="#.#%", animate=TRUE),
                    htmlOutput("intensity_F_message"),
                    tags$style(type="text/css", HTML("#intensity_F_message {color: #f50000;}"))
                ),
                
                h4("Doubling times"),
                dblt_picker(1, unit = "hour"),
                dblt_picker(2, unit = "day"),
                dblt_picker(3, unit = "month"),
                dblt_picker(4, unit = "year"),
                dblt_picker(5, value = 10, unit = "year")
            ),
            
            # Main panel / output
            mainPanel(
                tags$head(tags$style(type="text/css", ".tab-content {overflow: visible;}")),
                progressInit(),
                
                tabsetPanel(
                    tabPanel("Labeling Times", 
                             plotOutput("plot", height="600px"),
                             downloadButton('downloadPlot', 'Download Plot')), 
                    tabPanel("Enrichment Curves", 
                             radioButtons(
                                 "plot2DataType", "Y-axis in:",
                                 c(`Enrichment [in permil]` = "permil",
                                   `Total abundance [% light isotope]` = "frac"), selected="permil"),
                             checkboxInput("plot2Y", "Log scale", FALSE),
                             plotOutput("plot2", height="500px"),
                             sliderInput("plot2Xzoom", "Time scale:",
                                         min = 0.001, max = 1, step = 0.001, value = 1, 
                                         format="#.#%")),
                    tabPanel("Summary Table", 
                             h4("Enrichment of different doubling times as function of label strength and incubation time"), 
                             radioButtons(
                                 "tableDataType", "Report data in:",
                                 c(`Enrichment [in permil]` = "permil",
                                   `Total abundance [% light isotope]` = "frac"), selected="permil"),
                             tableOutput("table"),
                             downloadButton('downloadTable', 'Download Table')),
                    selected = "Labeling Times", position = "above")
            ), 
            
            position = "left"
        ),        
    div("Written by ", a(href="mailto:skopf@caltech.edu", "Sebastian Kopf"), br(), 
        "Powered by ", a(href="http://www.rstudio.com/", "RStudio"), "'s ", a(href="http://www.rstudio.com/shiny/", "Shiny engine"), br(),
        "Comes with absolutely no warranty", br(),
        "Released under ", a(href="http://www.gnu.org/licenses/gpl-3.0.html", "GPL-3"), align="right")
))