library(shiny)
library(shinydashboard)
library(shinyBS)
library(readxl)
library(dplyr)

# read parameters for axis options
params <- read_excel("nox_isotope_dynamics.xlsx", sheet = "variables")
axis_options <- with(
  params %>% subset(type %in% c("var", "inst")) %>% 
   transform(latex = paste0("\\(", gsub("\\\\\\\\","\\\\", latex),"\\)")), 
  setNames(id, latex))
value_inputs <- params %>% subset(adjustable == "yes") %>% select(id, name, unit, min, max, default) %>% 
  t() %>% as.data.frame(stringsAsFactors=FALSE) %>% as.list() %>% 
  lapply(function(i) numericInput(paste0("value_", i[1]), paste0(i[2], " [", i[3], "]"), value = as.numeric(i[6])))

procs <- read_excel("nox_isotope_dynamics.xlsx", sheet = "processes")
proc_options <- with(procs, setNames(as.list(call), name))


header <- dashboardHeader(title = "N-cycle isotopes")
sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  
  bsModal("save_dialog", "Save plot", "download", size = "small",
          textInput("save_name", "Filename:", "ncycle_arrow_plot.pdf"),
          numericInput("save_width", "Width [inches]:", 12),
          numericInput("save_height", "Height [inches]:", 8),
          downloadButton("save", "Save", icon("save"))
  ),
  
  fluidRow(
  
    # plot box
    box(
      div(align = "right", 
        actionLink("refresh", "Refresh", icon = icon("gear"), style = "padding-right:1em; padding-left:1em;"), 
        bsTooltip("refresh", "Refresh the plot"),
        actionLink("download", "Save", icon = icon("download")),
        bsTooltip("download", "Download the plot as a PDF")
      ),
      status = "primary", solidHeader = TRUE, width = 9,
      plotOutput("main_plot", height = "100%"),
      br()
    ),
    
    column(width = 3,
           # plot settings box
           box(title = "Settings", solidHeader = TRUE, collapsible = TRUE, status = "success", collapsed = TRUE, width = 12,
               div( style = "",
                 div(style="display:inline-block;",tags$label("Height:")),
                 div(style="display:inline-block; width: 60px;",
                     tags$input(id = "main_plot_height", type = "number", value = 400, min = 100, step = 50, class = "form-control")),
                 div(style="display:inline-block;",tags$label("px"))
               ),
               div(style = "",
                 div(style="display:inline-block;",tags$label("Model:")),
                 div(style="display:inline-block; width: 60px;",
                     tags$input(id = "model_steps", type = "number", value = 15, min = 5, step = 5, class = "form-control")),
                 div(style="display:inline-block;",tags$label("steps"))
               ),
               radioButtons("legend", "Legend Position", c("Right" = "right", "Below" = "below"), selected = "right", inline = TRUE),
               radioButtons("layout", "Layout", c("Grid" = "grid", "Panels" = "wrap"), selected = "grid", inline = TRUE)
           ),
           
           # default values
           box(title = "Processes", solidHeader = TRUE, collapsible = TRUE, status = "primary", collapsed = TRUE, width = 12,
               checkboxGroupInput("procs", "Processes:", procs$name, selected = procs$name)),
           
           # values
           box(title = "Parameters", solidHeader = TRUE, collapsible = TRUE, status = "danger", collapsed = TRUE, width = 12,
               numericInput("mass_flux", "Mass flux [meq/L]:", 1.0),
               value_inputs),

           # axes
           box(title = "Display", solidHeader = TRUE, collapsible = TRUE, status = "warning", collapsed = FALSE, width = 12,
               withMathJax(),
               fluidRow(
                 column(width = 5,
                        checkboxGroupInput("y_axis", "Y-axis:",
                                           axis_options)
                 ),
                 column(width = 5,
                        checkboxGroupInput("x_axis", "X-axis:",
                                           axis_options)
                 )
               ) 
           )
  
           
    )
    
  )
)

dashboardPage(header, sidebar, body)