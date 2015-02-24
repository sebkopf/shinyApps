library(shiny)
library(shinyFiles)
library(rCharts)
#options(RCHART_LIB = 'polycharts')

# Define UI that plots the isotope label enrichment 
shinyUI(
  fluidPage(
    
    # Application headers
    titlePanel("Sigman Lab Linearity - Data Processing"),
    "This user interface is intended to facility looking at linearity and ON/OFF test data and logging the results",
    br(),
    
    # File selectoin
    shinyFilesButton('files', 'Select files', 'Please select the linearity and all ON/OFF files', TRUE),
    hr(),

    # Tabs
    tabsetPanel(
      id = "tabs", selected = "file_tab", position = "above", type = "tabs",
      
      # File Details
      tabPanel(
        value = "file_tab", "File details", 
        htmlOutput("loaded_files"),
        plotOutput("mass_plot", height="600px", width = "800px")
        #showOutput("massPlot", "morris") # not using this morris plot at the moment because it's too slow
      ),
      
      # Linearity
      tabPanel(
        value = "linearity_tab", "Linearity",        
        br(),
        fluidRow(align="center",
          column(width = 1, "Range:"),
          column(width = 2, htmlOutput("slider_O_min")),
          column(width = 2, htmlOutput("slider_O_max")),
          column(width = 1, offset = 1, "Range:"),
          column(width = 2, htmlOutput("slider_N_min")),
          column(width = 2, htmlOutput("slider_N_max"))
        ),
        br(),
        fluidRow(align="center",
          column(width = 6, textOutput("regression_O")),
          column(width = 6, textOutput("regression_N"))
        ),
        fluidRow(align="center",
          column(width = 6, plotOutput("linearity_plot_O", width="400px")),
          column(width = 6, plotOutput("linearity_plot_N", width="400px"))
        ),
        br(),
        downloadButton("summarize", "Generate Summary", icon("save"))
      )
    )
  )
)
