library(shiny)
library(shinyFiles)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(lattice)
library(scales)
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
  get_linearity_mass_traces <- reactive(get_mass_traces(get_linearity_files()))
  
  # show linearity traces
  output$loaded_masses <- renderUI(make_trace_selector("selected_mass", get_linearity_files()))
  output$loaded_files <- renderUI(make_file_selector("selected_file", get_linearity_files(), selected = "linearity"))
  make_linearity_traces_plot <- reactive(
    if (is_linearity_loaded()) {
      withProgress(message = 'Rendering plot', detail = "for raw mass traces...", value = 0.5, 
                   plot_masses(get_linearity_mass_traces(), input$selected_file, input$selected_mass))
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
 
  # summary
  output$summarize <- downloadHandler(
    filename = function() {paste0(basename(get_linearity_folder()), "_summary.pdf")}, 
    content = function(file) { 
      withProgress(message = 'Generating summary', detail = "for linearity and ON/OFF data...", value = 0.5,             
                   generate_linearity_summary (
                     file.path(data_dir, get_linearity_folder()), 
                     get_linearity_data_table(), 
                     get_regression_O(), get_regression_N(),
                     make_linearity_plot_O(), make_linearity_plot_N(), 
                     save_download_file = file, summary_dir = data_dir)
      )
    })
  
  # history
  get_linearity_history <- reactive({
    get_linearity_folder() # make sure to trigger whenever there is a new folder loaded or tabs are changed
    
    if (input$linearity_tabs == "linearity_history_tab") { 
      # makes sure to trigger on tab change but only retrieve data if it's actually the right tab
      
      summary_file <- file.path(data_dir, linearity_record_csv)
      if (file.exists(summary_file)) {
        data <- read.csv(file.path(data_dir, linearity_record_csv), check.names = F, stringsAsFactors = F)
        data <- mutate(data, datetime = as.POSIXct(`Run date & time`), date = as.Date(datetime))
        
        # remove duplicates and sort
        data.nodup <- subset(data[rev(order(data$Timestamp)),], !duplicated(datetime))
        data.nodup <- data.nodup[order(data.nodup$datetime),] # sort by date time
        
        if (nrow(data.nodup) < nrow(data)) {
          # some duplicates removed --> store again
          message("Removing duplicates from history...")
          write.table(data.nodup[!names(data.nodup) %in% c("datetime", "date")], file = summary_file, row.names = FALSE, sep = ",", col.names = TRUE)
        }
        
        return(data.nodup)
      } else
        stop("No linearity history file yet stored at '", summary_file, "'")
    } else
      return(data.frame())
  })
  
  output$linhis_date_range_widget <- renderUI({
    if (nrow(data <- get_linearity_history()) > 0) {
      dateRangeInput("linhis_date_range", "", 
                     start = min(data$date)[1], end = max(data$date)[1],
                     min = min(data$date)[1], max = max(data$date)[1],
                     format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                     language = "en", separator = " to ")
    }
  })
  
  make_linearity_history_plot <- reactive({
    
    # show if any data selected in the date range
    # doing the sequential && to trigger the right reactivity 
    if (nrow(data <- get_linearity_history()) > 0 &&
          nrow(data <- subset(data, 
            date >= input$linhis_date_range[1] & date <= input$linhis_date_range[2])) > 0) {
      
      message("Plotting linearity history from ", input$linhis_date_range[1], " to ", input$linhis_date_range[2])
      withProgress(message = 'Rendering plot', detail = "for linearity history...", value = 0.5, {
        data.melt <- melt(data[c("date", "Linearity d15N slope [permil/V]", "Linearity d18O slope [permil/V]")],
                          id.vars = "date")  
        ggplot(data.melt, aes(date, value, fill = variable)) + 
          geom_point(shape = 21, size = 4) +
          scale_x_date("Date", labels = date_format("%b %d\n%Y")) + 
          labs(y = "linearity slope [permil/V]", fill = "") + 
          theme_bw() +
          theme(text = element_text(size = 18), 
                legend.position = "bottom", legend.direction = "vertical") 
      })
    } else 
      plot.new()
  })
  output$linearity_history <- renderPlot(make_linearity_history_plot())
  
  
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