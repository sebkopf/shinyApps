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
                htmlOutput("nat"),
                
                h5("Label strengths"),
                sliderInput("label1", "",
                            min = 0, max = 0.1, step = 0.0001, value = 0.01, 
                            format="#.##%"),
                htmlOutput("label1_message"),
                tags$style(type="text/css", HTML("#label1_message {color: #f50000;}")),
                sliderInput("label2", "", 
                            min = 0, max = 0.5, step = 0.0005, value = 0.1, 
                            format="#.##%"),
                htmlOutput("label2_message"),
                tags$style(type="text/css", HTML("#label2_message {color: #f50000;}")),
                sliderInput("label3", "", 
                            min = 0, max = 1, step = 0.001, value = 0.5, 
                            format="#.#%"),
                htmlOutput("label3_message"),
                tags$style(type="text/css", HTML("#label3_message {color: #f50000;}")),
                checkboxGroupInput(inputId="show", label="Show labeling strengths:", choices=1:3, selected=1:3),
                tags$style(type="text/css", HTML("#show>*{float: left; margin-right: 15px; height: 20px;} #show {height: 20px;}")),
                
                h5("Target enrichment"),
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
                progressInit(),
                
                tabsetPanel(
                    tabPanel("Plot", 
                             plotOutput("plot", height="600px"),
                             downloadButton('downloadPlot', 'Download Plot')), 
                    tabPanel("Table", 
                             h4("Enrichment of different doubling times as function of label strength and incubation time"), 
                             radioButtons(
                                 "tableDataType", "Report data in:",
                                 c(`Enrichment [in permil]` = "permil",
                                   `Total abundance [% light isotope]` = "frac"), selected="permil"),
                             tableOutput("table"),
                             downloadButton('downloadTable', 'Download Table')),
                    selected = "Table")
            ), 
            
            position = "left"
        ),        
    div("Written by ", a(href="mailto:skopf@caltech.edu", "Sebastian Kopf"), br(), 
        "Powered by ", a(href="http://www.rstudio.com/", "RStudio"), "'s' ", a(href="http://www.rstudio.com/shiny/", "Shiny engine"), br(),
        "Comes with absolutely no warranty", br(),
        "Released under ", a(href="http://www.gnu.org/licenses/gpl-3.0.html", "GPL-3"), align="right")
))