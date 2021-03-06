library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
source("widgets.R")

header <- dashboardHeader(title = "Isotope calculator")


##### SIDEBAR ######

sidebar <- dashboardSidebar(
  sidebarMenu( id = "tabs",
    menuItem("Labeling times", tabName = "plot1", icon = icon("clock-o"), selected = TRUE),
    menuItem("Enrichment curves", tabName = "plot2", icon = icon("line-chart")),
    menuItem("Summary table", tabName = "table", icon = icon("table")),
    menuItem("About", tabName = "about", icon = icon("cog"))
  )
)

##### MAIN #####

body <- dashboardBody(
  
  ##### SAVE DIALOGS #####
  
  bsModal("save_dialog", "Save plot", "downloadPlot", size = "small",
          textInput("save_name", "Filename:", "isolabel_plot_times.pdf"),
          numericInput("save_width", "Width [inches]:", 12),
          numericInput("save_height", "Height [inches]:", 8),
          downloadButton("save", "Save", icon("save"))
  ),
  
  bsModal("save_dialog2", "Save plot", "downloadPlot2", size = "small",
          textInput("save_name2", "Filename:", "isolabel_plot_curves.pdf"),
          numericInput("save_width2", "Width [inches]:", 12),
          numericInput("save_height2", "Height [inches]:", 8),
          downloadButton("save2", "Save", icon("save"))
  ),
  
  bsModal("save_dialog3", "Save table", "downloadTable", size = "small",
          textInput("save_name3", "Filename:", "isolabel_table.csv"),
          downloadButton("save3", "Save", icon("save"))
  ),
  
  ##### OUTPUTS ####
  
  fluidRow(
    column(width = 8, 
          
           ### plot 1 ###
           tabItems(
             tabItem(tabName = "plot1",
                     box(
                       div(align = "right", 
                           actionLink("updatePlot1", "Refresh", icon = icon("gear"), 
                                      style = "padding-right:1em; padding-left:1em;"), 
                           bsTooltip("updatePlot1", "Refresh the plot"),
                           actionLink("downloadPlot", "Save plot", icon = icon("download")),
                           bsTooltip("downloadPlot", "Download the plot as a PDF")
                       ),
                       status = "primary", solidHeader = TRUE, width = 12,
                       plotOutput("plot", height = "100%")
                     )
             ),
             
             ### plot 2 ###
             tabItem(tabName = "plot2",
                     box(
                       div(align = "right", 
                           actionLink("updatePlot2", "Refresh", icon = icon("gear"), 
                                      style = "padding-right:1em; padding-left:1em;"), 
                           bsTooltip("updatePlot2", "Refresh the plot"),
                           actionLink("downloadPlot2", "Save plot", icon = icon("download")),
                           bsTooltip("downloadPlot2", "Download the plot as a PDF")
                       ),
                       status = "primary", solidHeader = TRUE, width = 12,
                       radioButtons(
                         "plot2DataType", "",
                         c(`Enrichment [in permil]` = "permil",
                           `Total abundance [at% rare isotope]` = "frac"), selected="permil"),
                       plotOutput("plot2", height = "100%"))
             ),
                  
             ### table ###    
             tabItem(tabName = "table", 
                     box(
                       div(align = "right", 
                           actionLink("updateTable", "Refresh", icon = icon("gear"), 
                                      style = "padding-right:1em; padding-left:1em;"), 
                           bsTooltip("updateTable", "Refresh the table"),
                           actionLink("downloadTable", "Save table", icon = icon("download")),
                           bsTooltip("downloadTable", "Download the table as a CSV file")
                       ),
                       status = "primary", solidHeader = TRUE, width = 12,         
                       
                       h4("Enrichment of different generation times as function of label strength and incubation time"), 
                       radioButtons(
                         "tableDataType", "Report data in:",
                         c(`Enrichment [in permil]` = "permil",
                           `Total abundance [at% rare isotope]` = "frac"), selected="permil"),
                       br(),
                       tableOutput("table")
                     )
                     
             ),
             
             ### about ###    
             tabItem(
               tabName = "about", 
               box(title = "About", status = "success", solidHeader = TRUE, width = 12,
                   h2("Isotope labeling calculator"),
                   
                   withMathJax(),
                   h3("This app is intended for use in ", em("estimating"), " expected enrichments from stable isotope labeling in microbial populations. It is based on isotopic enrichment resulting from unbiased clonal growth in the presence of an isotopic label: $$F{\\left(t\\right)}=F_{N}\\left(1-e^{-{\\mu}\\cdot{t}}\\right)+F_{t_0}\\cdot{e^{-{\\mu}\\cdot{t}}}
\\left(\\mu{}=\\ln(2)/T\\right)$$ with \\({F_{N}}\\) the isotopic composition of new biomass generated in the presence of the isotope label (using fractional abundances \\(F\\) is important for correct mass balance!). Substituting the growth rate for the generation time \\(\\left(µ = \\ln(2) / T\\right)\\), one can readily calculate the required labeling time to reach a target isotopic composition \\({F_{f}}\\): $$t_{label}=\\frac{T}{ln(2)}\\cdot{\\ln\\left(\\frac{F_N-F_{t_0}}{F_N-F_f}\\right)}$$ This app is intended to make it easy to explore this space for different isotope systems and takes care of all the isotope conversions and mixing calculations between existing and isotopically labelled pools."),
                   
                   br(),
                   h2("Source code"),
                   h3("This app was written by ", a(href="mailto:sebastian.kopf@colorado.edu", "Sebastian Kopf"), " and is powered by ", a(href="http://www.rstudio.com/", "RStudio"), "'s ", a(href="http://www.rstudio.com/shiny/", "Shiny engine"), ".", "The source code for this application is released under ", a(href="http://www.gnu.org/licenses/gpl-3.0.html", "GPL-3")," and is available on ", a(href="https://www.github.com/sebkopf/shinyApps/tree/master/isolabel", "GitHub"), ".", "Please use the repository's", a(href="https://github.com/sebkopf/shinyApps/issues", "Issue Tracker"), "for any feedback, suggestions and bug reports."
                   )
               )
             )
           )
          
    ),

    ##### INPUTS #####
    
    column(width = 4,
           
           ### plot options ###
           
           box(title = "Plot Options", 
               solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = TRUE, width = 12,
               
               generate_input_row("main_plot_height", "Height:", post = "px", value = 600, min = 100, step = 50),
               
               radioButtons("legend", "Legend Position:", c("Right" = "right", "Below" = "below"), 
                            selected = "right", inline = TRUE),
               shinyjs::useShinyjs(),
               shinyjs::hidden(
                 div(id = "plot_settings_div",
                     sliderInput("plot2Yzoom", "Y-axis (enrichment) zoom:",
                                 min = 0.1, max = 100, step = 0.1, value = 100, 
                                 post = "%"),
                     
                     sliderInput("plot2Xzoom", "X-axis (time scale) zoom:",
                                 min = 0.1, max = 100, step = 0.1, value = 100, 
                                 post = "%")
                 ))
           ),
           
           ### isotope labels ###
           
           box(title = "Isotope system", 
               solidHeader = TRUE, collapsible = TRUE, status = "warning", collapsed = FALSE, width = 12,   
               h4(selectInput("ref", NULL, choices = c("2H", "13C", "15N", "18O", "34S"))),
               htmlOutput("nat"),
               htmlOutput("rare_iso_header"),
               generate_input_row("label.ref_vol", "Initial volume:", value = 1000, step = 50, min = 0),
               generate_input_row("label.ref_conc", "Initial concentration:", value = 1, step = 0.1, min = 0),
               br(),
               strong("Target enrichment:"),
               radioButtons(
                 "targetType", NULL,
                 c(`Relative enrichment [in permil]` = "permil",
                   `Total abundance [at% rare isotope]` = "frac"), selected="permil"),
               
               conditionalPanel(
                 condition = "input.targetType == 'permil'",
                 numericInput("intensity_permil", NULL, 500, min = 0, step = 100)
               ),
               
               conditionalPanel(
                 condition = "input.targetType == 'frac'",
                 sliderInput("intensity_F", NULL, post = "%",
                             min = 0, max = 100, step = 0.1, value = 1.5), #animate=TRUE),
                 htmlOutput("intensity_F_message"),
                 tags$style(type="text/css", HTML("#intensity_F_message {color: #f50000;}"))
               ),
               
               h3("Isotope label strengths:"),
               htmlOutput("iso_labels_error"),
               tags$style(type="text/css", HTML("#iso_labels_error {color: #f50000;}")),
               DT::dataTableOutput('iso_labels'),
               shinyjs::hidden(
                 div(id = "iso_labels_editbox_div",
                     h4("Edit isotope label"),
                     checkboxInput("iso_label_edit_include", "include in plots and tables"),
                     generate_input_row("iso_label_edit_vol", "Volume:", value = 1, step = 1, min = 0), 
                     generate_input_row("iso_label_edit_conc", "Concentration:", value = 1, step = 0.1, min = 0),
                     sliderInput("iso_label_edit_spike", NULL, min = 0, max = 100, step = 1, value = 1, post = " at%")
                 )
               )
           ),
           
           
           ### generation times ###
           
           box(title = "Generation times", 
               solidHeader = TRUE, collapsible = TRUE, status = "danger", collapsed = FALSE, width = 12,
               DT::dataTableOutput('gen_times'),
               shinyjs::hidden(
                 div(id = "gen_editbox_div",
                     h4("Edit generation time"),
                     checkboxInput("dblt_edit_include", "include in plots and tables"),
                     fluidRow(
                       column(4, offset = 1, numericInput("dblt_edit_value", NULL, 1, min = 1, step = 1)),
                       column(6, offset = 0, selectInput(
                         "dblt_edit_units", NULL, selected = "",
                         choices = c("minute", "hour", "day", "week", "month", "year")))
                     )
                 )
               )
           )
           
    )
  )
)

dashboardPage(header, sidebar, body)
    

