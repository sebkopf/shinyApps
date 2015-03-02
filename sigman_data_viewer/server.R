library(shiny)
library(shinyFiles)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lattice)
library(isoread)
library(plyr)
#library(rCharts) 
#options(RCHART_WIDTH = 800)

source("utils.R")
source("settings.R")
source("linearity.R")

# SERVER =====
shinyServer(function(input, output, session) {
  
  
  # SETTINGS =======
  settings <- read.csv("settings.csv", stringsAsFactors = FALSE)
  data_dir <- subset(settings, Variable == "data_dir")$Value # root directory for the file browser
  message("Launching data viewer with base data directy '", data_dir, "'")
  
  get_settings <- reactive({
    sets <- settings
    msg <- ""
    if (input$save_settings > 0) { 
      saved <- isolate(save_settings(settings, input))
      sets <- saved$settings
      msg <- saved$msg
    }
    return (c(dlply(sets, .(Variable), function(df) df$Value), list(msg = msg)))
  })
  output$settings <- renderUI(make_settings_UI(settings))
  output$settings_msg <- renderUI(HTML(get_settings()$msg))
  
  
  
#   shinyFileChoose(input, 'files', session = session, roots=c(wd=data_dir),
#                   filetypes=c('dxf'))
  
  
  # LINEARITY ============
  shinyFileChoose(input, 'linearity_folder', session = session, roots=c(wd=data_dir),
                  filetypes=c('NONEALLOWED'))

  # load data
  is_linearity_loaded <- reactive(!is.null(get_linearity_folder()))
  get_linearity_folder <- reactive({
    files <- parseFilePaths(data_dir, input$linearity_folder)
    if (nrow(files) == 0) return(NULL)
    return(sub("^(NA|\\.)?(.*)$", ".\\2", files[1,"datapath"]))
  })
  get_linearity_files <- reactive({
    if ( is_linearity_loaded() ) { 
      return(
        withProgress(message = 'Loading data...', value = 0, {
            load_isodat_files (
              list.files(file.path(data_dir, get_linearity_folder()), pattern = "\\.dxf$", full.names = TRUE), 
              function(file, n) incProgress(1/n, detail = paste0("Reading ", file, " ...")))
          })
      )
    } else 
      return(list())
  })
  get_linearity_data_table <- reactive(get_data_tables(get_linearity_files()))
  
  # show linearity traces
  output$loaded_masses <- renderUI(make_trace_selector("selected_mass", get_linearity_files()))
  output$loaded_files <- renderUI(make_file_selector("selected_file", get_linearity_files(), selected = "linearity"))
  make_linearity_traces_plot <- reactive(
    if (is_linearity_loaded()) {
      withProgress(message = 'Rendering plot', detail = "for raw mass traces...", value = 0.5, 
                   plot_masses(get_linearity_files(), input$selected_file, input$selected_mass))
    }
  )
  output$linearity_traces_plot <- renderPlot(make_linearity_traces_plot())
  
  # linearity evaluation
  get_linearity_data_O <- reactive(get_linearity_plot_data(get_linearity_data_table(), " 18O/16O"))
  get_linearity_data_N <- reactive(get_linearity_plot_data(get_linearity_data_table(), " 15N/14N"))

  # cutoffs
  output$slider_O_min <- renderUI(make_cutoff_slider("cutoff_O_min", get_settings(), max = ceiling(max(get_linearity_data_O()$x)))) 
  output$slider_O_max <- renderUI(make_cutoff_slider("cutoff_O_max", get_settings(), max = ceiling(max(get_linearity_data_O()$x))))   
  output$slider_N_min <- renderUI(make_cutoff_slider("cutoff_N_min", get_settings(), max = ceiling(max(get_linearity_data_N()$x)))) 
  output$slider_N_max <- renderUI(make_cutoff_slider("cutoff_N_max", get_settings(), max = ceiling(max(get_linearity_data_N()$x))))     
  get_xrange_O <- reactive(c(input$cutoff_O_min, input$cutoff_O_max)) 
  get_xrange_N <- reactive(c(input$cutoff_N_min, input$cutoff_N_max)) 

  # regressions
  get_regression_O <- reactive(get_linearity_reg("d18O", get_linearity_data_O(), get_xrange_O()))
  output$regression_O <- renderText(if (is_linearity_loaded() && length(get_xrange_O()) > 0) isolate(get_regression_O()$msg))
  get_regression_N <- reactive(get_linearity_reg("d15N", get_linearity_data_N(), get_xrange_N()))
  output$regression_N <- renderText(if (is_linearity_loaded() && length(get_xrange_N()) > 0) isolate(get_regression_N()$msg))

  # plots
  make_linearity_plot_O <- reactive(make_linearity_plot("d18O [permil]", get_linearity_data_O(), get_xrange_O()))
  output$linearity_plot_O <- renderPlot(if (is_linearity_loaded() && length(get_xrange_O()) > 0) isolate(print(make_linearity_plot_O())))
  make_linearity_plot_N <- reactive(make_linearity_plot("d15N [permil]", get_linearity_data_N(), get_xrange_N()))
  output$linearity_plot_N <- renderPlot(if (is_linearity_loaded() && length(get_xrange_N()) > 0) isolate(print(make_linearity_plot_N())))
 
  # SUMMARY ========
  output$summarize <- downloadHandler(
    filename = function() {paste0(basename(get_linearity_folder()), "_summary.pdf")}, 
    content = function(file) { 
      withProgress(message = 'Generating summary', detail = "for linearity and ON/OFF data...", value = 0.5,             
                   generate_linearity_summary (
                     get_linearity_folder(), get_linearity_data_table(), 
                     get_regression_O(), get_regression_N(),
                     make_linearity_plot_O(), make_linearity_plot_N(), 
                     save_download_file = file, summary_dir = data_dir)
      )
    })
  
  
#   # MORRIS chart (interactive rchart) - but is too slow
#   output$massPlot <- renderChart2({
#     iso_file <- isoread(file.path(data_dir, "MAT25391411_ON OFFS_linearity.dxf"), type = "CFLOW", read_mass_data = TRUE)
#     
#     m1 <- mPlot(x="time", y=c("mass44", "mass45", "mass46"), data = 
#                   mutate(iso_file$massData, time = round(time), mass44 = round(mass44), mass45 = round(mass45), mass46 = round(mass46)), 
#                 type="Line", parseTime=FALSE)
#     m1$set(pointSize = 0, lineWidth = 1)
#     #m1$set(height=500 width = 500) # works
#     return(m1)
#   })

})