library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(readxl)
library(magrittr)
library(plyr)

# read parameters for axis options
params <- read_excel("nox_isotope_dynamics.xlsx", sheet = "variables")
axis_options <- with(
  params %>% subset(type %in% c("var", "inst")) %>% 
   transform(latex = paste0("\\(", gsub("\\\\\\\\","\\\\", latex),"\\)")), 
  setNames(id, latex))
value_inputs <- params %>% subset(adjustable == "yes", select = c("id", "name", "unit", "min", "max", "default")) %>% 
  t() %>% as.data.frame(stringsAsFactors=FALSE) %>% as.list() %>% 
  lapply(function(i) numericInput(paste0("value_", i[1]), paste0(i[2], " [", i[3], "]"), value = as.numeric(i[6])))

procs <- read_excel("nox_isotope_dynamics.xlsx", sheet = "processes")
proc_options <- with(procs, setNames(as.list(call), name))


header <- dashboardHeader(title = "N-cycle isotopes")
sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  
  shinyjs::useShinyjs(),
  
  bsModal("save_dialog", "Save plot", "download", size = "small",
          textInput("save_name", "Filename:", "ncycle_arrow_plot.pdf"),
          numericInput("save_width", "Width [inches]:", 12),
          numericInput("save_height", "Height [inches]:", 8),
          downloadButton("save", "Save", icon("save"))
  ),
  
  
  fluidRow(
  
    column(width = 9, 
           
           # plot box
           box(
             div(align = "right", 
                 actionLink("refresh", "Refresh", icon = icon("gear"), style = "padding-right:1em; padding-left:1em;"), 
                 bsTooltip("refresh", "Refresh the plot"),
                 actionLink("download", "Save", icon = icon("download")),
                 bsTooltip("download", "Download the plot as a PDF")
             ),
             status = "primary", solidHeader = TRUE, width = 12,
             plotOutput("main_plot", height = "100%"),
             br()
           ),
           
           # parameter box
           box(title = "Parameters", solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = FALSE, width = 12,
               div(align = "right", 
                   actionLink("refresh2", "Refresh", icon = icon("gear"), style = "padding-right:1em; padding-left:1em;"), 
                   bsTooltip("refresh2", "Refresh the plot")
               ),
               fluidRow(
                 withMathJax(DT::dataTableOutput('parameters')) %>% column(width = 9),
                 fluidRow(
                   shinyjs::hidden(
                     div(id = "editbox_div",
                         br(),
                         h3("Edit parameter"),
                         h4(textOutput("editbox_name")),
                         numericInput("editbox_value", label = "", value = "")
                     )
                   )
                 ) %>% column(width = 2)
               )
           ),
           
           fluidRow(
             column(12, "Written by ", a(href="mailto:skopf@princeton.edu", "Sebastian Kopf"), 
                    " and powered by ", a(href="http://www.rstudio.com/", "RStudio"), "'s ", 
                    a(href="http://www.rstudio.com/shiny/", "Shiny engine"), "."),
             column(12, "The source code for this application is released under ", 
                    a(href="http://www.gnu.org/licenses/gpl-3.0.html", "GPL-3")," and is available on ",
                    a(href="https://www.github.com/sebkopf/shinyApps/tree/master/nitrogen_cycle_dynamics", "GitHub"), "."),
             column(12, "Please use the repository's",
                    a(href="https://github.com/sebkopf/shinyApps/issues", "Issue Tracker"),
                    "for any feedback, suggestions and bug reports.")
           )
    ),
    
    column(width = 3,
           
           # for parameter edit on the side bar
#            shinyjs::hidden(
#              div(id = "editbox_div", 
#                  box(title = "Parameter", solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = FALSE, width = 12,
#                      h4(textOutput("editbox_name")),
#                      numericInput("editbox_value", label = "", value = ""),
#                      h4(actionLink("editbox_submit", " Change value", icon = icon("save")))
#                  ))
#            ),
           
           # plot settings box
           box(title = "Display", solidHeader = TRUE, collapsible = TRUE, status = "danger", collapsed = TRUE, width = 12,
               div( style = "",
                 div(style="display:inline-block;",tags$label("Height:")),
                 div(style="display:inline-block; width: 60px;",
                     tags$input(id = "main_plot_height", type = "number", value = 500, min = 100, step = 50, class = "form-control")),
                 div(style="display:inline-block;",tags$label("px"))
               ),
               div(style = "",
                 div(style="display:inline-block;",tags$label("Model:")),
                 div(style="display:inline-block; width: 60px;",
                     tags$input(id = "model_steps", type = "number", value = 15, min = 5, step = 5, class = "form-control")),
                 div(style="display:inline-block;",tags$label("steps"))
               ),
               radioButtons("legend", "Legend Position", c("Right" = "right", "Below" = "below"), selected = "right", inline = TRUE)
               #radioButtons("layout", "Layout", c("Grid" = "grid", "Panels" = "wrap"), selected = "grid", inline = TRUE)
           ),
           
           # default values
           box(title = "Processes", solidHeader = TRUE, collapsible = TRUE, status = "primary", collapsed = FALSE, width = 12,
               DT::dataTableOutput('processes')),
           
           # axes
           box(title = "Axes", solidHeader = TRUE, collapsible = TRUE, status = "warning", collapsed = FALSE, width = 12,
               fluidRow(
                 column(6, withMathJax(DT::dataTableOutput('axis_y'))),
                 column(6, withMathJax(DT::dataTableOutput('axis_x')))
               )
           ),
           
           # somehow required for proper mathjax rendering in the data tables
           shinyjs::hidden(div(id = "test",checkboxGroupInput("axis_hidden", "hidden", axis_options)))
           
    )
    
  )
  
)

dashboardPage(header, sidebar, body)