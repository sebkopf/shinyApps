library(shiny)
library(shinyFiles)
library(rCharts)
#options(RCHART_LIB = 'polycharts')

# Define UI that plots the isotope label enrichment 
shinyUI(
  navbarPage(
    title = "Sigman Data Viewer",
    header = "",
    id = "menu", inverse = FALSE, 
    

    tabPanel("Linearity", value="linearity",
    
      # Tabs
      tabsetPanel(
        id = "linearity_tabs", selected = "linearity_history_tab", position = "above", type = "tabs",
        
        # Linearity history
        tabPanel(
          value = "linearity_history_tab", "History",        
          br(),
          fluidRow(column(width = 5, offset = 1, htmlOutput("linhis_date_range_widget"))),
          plotOutput("linearity_history", height="600px", width = "900px")
        ),
        
        # File Details
        tabPanel(
          value = "file_tab", 
          shinyFilesButton('linearity_folder', 'New Data', 'Please select the linearity & ON/OFF folder', 
                           multiple = FALSE, folder_select = TRUE, sort_by = "Name", ascending = FALSE), 
          br(),
          fluidRow(
            column(width = 3, htmlOutput("loaded_masses")),
            column(width = 9, htmlOutput("loaded_files"))
          ),
          plotOutput("linearity_traces_plot", height="500px", width = "900px")
          #showOutput("massPlot", "morris") # not using this morris plot at the moment because it's too slow
        ),
        
        
        # Linearity
        tabPanel(
          value = "linearity_tab", "Evaluation",        
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
          downloadButton("summarize", "Save record & generate summary", icon("save")),
          br(),
          ""
        )
      )
    ),
  
  # SETTINGS MENU ==========
  
  tabPanel(
    "Settings", value = "settings",
    h4("Please only edit these settings if you know what you're doing!", style = "color: #f50000;"),
    htmlOutput("settings"),
    br(),
    actionButton("save_settings", "Save settings", icon("save")),
    br(),br(),
    strong(htmlOutput("settings_msg"), style = "color: #f50000;")
  )
  )
)
