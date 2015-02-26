library(shiny)
library(shinyFiles)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
require(lattice)
library(isoread)
library(plyr)
#library(rCharts) 
#options(RCHART_WIDTH = 800)

# set root directory for file browser
root_dir <- '.'
O_cutoff <- c(0, 30) # [V]
N_cutoff <- c(4, 20) # [V]

# temp_files <- structure(list(name = structure(1:6, .Label = 
#                                                 c("MAT25391406_ON OFFS_ON OFF.dxf", 
#                                                   "MAT25391407_ON OFFS_ON OFF.dxf", "MAT25391408_ON OFFS_ON OFF.dxf", 
#                                                   "MAT25391409_ON OFFS_ON OFF.dxf", "MAT25391410_ON OFFS_ON OFF.dxf", 
#                                                   "MAT25391411_ON OFFS_linearity.dxf"), class = "factor"), 
#                              size = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), 
#                              type = structure(c(1L, 1L, 1L, 1L, 1L, 1L), class = "factor", .Label = ""), 
#                              datapath = structure(1:6, .Label = c("NA/150219_ON OFFS/MAT25391406_ON OFFS_ON OFF.dxf", "NA/150219_ON OFFS/MAT25391407_ON OFFS_ON OFF.dxf", "NA/150219_ON OFFS/MAT25391408_ON OFFS_ON OFF.dxf", 
#                                                                   "NA/150219_ON OFFS/MAT25391409_ON OFFS_ON OFF.dxf", "NA/150219_ON OFFS/MAT25391410_ON OFFS_ON OFF.dxf", 
#                                                                   "NA/150219_ON OFFS/MAT25391411_ON OFFS_linearity.dxf"), class = "factor")), 
#                         .Names = c("name", 
#                                    "size", "type", "datapath"), row.names = c("0", "1", "2", "3", 
#                                                                               "4", "5"), class = "data.frame")

server <- shinyServer(function(input, output, session) {
  shinyFileChoose(input, 'files', session = session, roots=c(wd=root_dir),
                  filetypes=c('dxf'))

  # get file folder
  get_isodat_folder <- reactive({
    files <- parseFilePaths(root_dir, input$files)
    #files <- temp_files # FIXME
    return(dirname(sub("^(NA|\\.)?(.*)$", ".\\2", files[1,"datapath"])))
  })
  
  # isodat files
  get_isodat_files <- reactive({
    files <- parseFilePaths(root_dir, input$files)
    #files <- temp_files # FIXME
    if (nrow(files) > 0) {
      
      iso_files <-
        withProgress(
          message = 'Loading data...', value = 0, {
            iso_files <- list()
            for (file in files$name) {
              incProgress(1/nrow(files), detail = paste0("Reading ", file, " ..."))
              iso_files <- c(iso_files, isoread(file.path(get_isodat_folder(), file), type = "CFLOW"))
            }
            return(iso_files)
          })
      
      
      names(iso_files) <- sapply(iso_files, function(i) i$filename)
      return(iso_files)
    } else {
      return(list())
    }
  })

  # get the data table
  get_data_table <- reactive({
    do.call(rbind, lapply(get_isodat_files(), function(i) mutate(i$get_data_table(), file =i$filename)))
  })
  
  # File Details Tab ======================
  
  output$loaded_files <- renderUI({ 
    files <- names(get_isodat_files())
    selectInput("selected_file", "", 
                if (length(files) > 0) files else c("No files loaded yet"="0"),
                selected = grep("linearity", files, value = T),
                multiple=FALSE, selectize=FALSE)
  })

  # Overview plot
  make_mass_plot <- reactive({
    file <- input$selected_file
    if (length(file) > 0 && file != "0") {
      # only update if selected_file changes
      message("Showing plot from ", file)
      isolate({
        get_isodat_files()[[file]]$plot()
      })
    } else {
      plot.new()
    }
  })
  
  output$mass_plot <- renderPlot({ make_mass_plot() })
  
  # Linearity Tab ======================
  
  linearity_base_plot <-
    ggplot(NULL, aes(x, y, fill = !include, shape = !include)) + 
    scale_shape_manual (values = c(21, 22)) + 
    scale_fill_manual (values = brewer.pal(9, "Set1")[c(2,1)]) + 
    theme_bw() + theme(legend.position = "none", axis.title = element_text(size = 20)) +
    labs(x = "Intensity 45 [V]")
  
  
  # Oxygen
  
  get_linearity_data_O <- reactive({
    mutate(subset(get_data_table(), grepl("linearity", file)),
           x = `rIntensity 45`/10000, # FIXME, should be amplitude
           y = ` 18O/16O`)
  })
  
  output$slider_O_min <- renderUI({ 
    sliderInput("range_O_min", "", min = 0, max = ceiling(max(get_linearity_data_O()$x)), step = 1, value = O_cutoff[1], post = " V")
  })
  
  output$slider_O_max <- renderUI({ 
    sliderInput("range_O_max", "", min = 0, max = ceiling(max(get_linearity_data_O()$x)), step = 1, value = ceiling(max(get_linearity_data_O()$x)), post = " V")
  })
  
  get_cutoff_O <- reactive({
    c(input$range_O_min, input$range_O_max)*10000 # FIXME
  })
  
  get_cutoff_linearity_data_O <- reactive({
    cutoff <- get_cutoff_O()
    mutate(get_linearity_data_O(),
           include = (`rIntensity 45` > cutoff[1] & `rIntensity 45` < cutoff[2]))
  })
  
  get_regression_O <- reactive({
    m <- lm(y ~ x, data = subset(get_cutoff_linearity_data_O(), include))
    ml <- list(slope = signif(coef(m)[['x']], 3), r2 = sprintf("%.2f", summary(m)$r.squared))
    return(paste0("d18O slope: ", ml$slope, " permil/V (R2: ", ml$r2, ")"))
  })

  output$regression_O <- renderText({
    if (length(get_cutoff_O()) > 0) {
      isolate(get_regression_O())
    } 
  })

  make_linearity_plot_O <- reactive({
    plot.df <- get_cutoff_linearity_data_O()
    linearity_base_plot %+% plot.df + 
      stat_smooth(data = subset(plot.df, include), method = lm) + 
      geom_point(size = 4) +
      labs(y = "d 18O/16O [permil]")
  })
  
  output$linearity_plot_O <- renderPlot({ 
    if (length(get_cutoff_O()) > 0)
      isolate({ # have to isolate so the regression doesn't get calculated without cutoffs
        print(make_linearity_plot_O()) 
      })
  })
  
  # Nitrogen

  get_linearity_data_N <- reactive({
    mutate(subset(get_data_table(), grepl("linearity", file)),
           x = `rIntensity 45`/10000, # FIXME, should be amplitude
           y = ` 15N/14N`)
  })

  output$slider_N_min <- renderUI({ 
    sliderInput("range_N_min", "", min = 0, max = ceiling(max(get_linearity_data_N()$x)), step = 1, value = N_cutoff[1], post = " V")
  })
  
  output$slider_N_max <- renderUI({ 
    sliderInput("range_N_max", "", min = 0, max = ceiling(max(get_linearity_data_N()$x)), step = 1, value = N_cutoff[2], post = " V")
  })
  
  get_cutoff_N <- reactive({
    c(input$range_N_min, input$range_N_max)*10000 # FIXME
  })
  
  get_cutoff_linearity_data_N <- reactive({
    cutoff <- get_cutoff_N()
    mutate(get_linearity_data_N(),
           include = (`rIntensity 45` > cutoff[1] & `rIntensity 45` < cutoff[2]))
  })
  
  get_regression_N <- reactive({
    m <- lm(y ~ x, data = subset(get_cutoff_linearity_data_N(), include))
    ml <- list(slope = signif(coef(m)[['x']], 3), r2 = sprintf("%.2f", summary(m)$r.squared))
    return(paste0("d15N slope: ", ml$slope, " permil/V (R2: ", ml$r2, ")"))
  })
  
  output$regression_N <- renderText({
    if (length(get_cutoff_N()) > 0) {
      isolate(get_regression_N())
    } 
  })
  
  make_linearity_plot_N <- reactive({
    plot.df <- get_cutoff_linearity_data_N()
    linearity_base_plot %+% plot.df + 
      stat_smooth(data = subset(plot.df, include), method = lm) + 
      geom_point(size = 4) +
      labs(y = "d 15N/14N [permil]")
  })
  
  output$linearity_plot_N <- renderPlot({ 
    if (length(get_cutoff_N()) > 0)
      isolate({ # have to isolate so the regression doesn't get calculated without cutoffs
        print(make_linearity_plot_N()) 
      })
  })
  
  # SUMMARY ========

  get_on_off_table <- reactive({
    ddply(
      subset(get_data_table(), grepl("ON OFF.dxf", file)),
      .(file),
      summarise,
      `ON/OFF File` = sub("^(MAT\\d+).*", "\\1", unique(file)),
      `Std. Dev. d18O` = signif(sd(` 18O/16O`), 3),
      `Std. Dev. d15N` = signif(sd(` 15N/14N`), 3)
    )[-1]
  })
  
  generate_summary <- function(file) {
    pdf(file, width = 8, height = 4)
    grid.arrange(
      main= paste0("\nSummary for ON/OFF and linearity test for folder '", basename(get_isodat_folder()), "'"),
      tableGrob(get_on_off_table(), 
                gpar.coltext = gpar(fontsize=8, fontface="bold"),
                gpar.coretext = gpar(fontsize=8),
                show.rowname = FALSE), 
      make_linearity_plot_O() + theme(axis.title = element_text(size = 10)), 
      make_linearity_plot_N() + theme(axis.title = element_text(size = 10)),
      nrow=1, as.table=FALSE, 
      sub=paste("\n", get_regression_O(), "\n", get_regression_N(), "\n"))
    dev.off()
  }
  
  output$summarize <- downloadHandler(
    filename = function() {paste0(basename(get_isodat_folder()), "_summary.pdf")}, 
    content = function(file) { 
      message("Generating summary")
      generate_summary(file.path( # save loacally
        get_isodat_folder(),
        paste0(basename(get_isodat_folder()), "_summary.pdf")
        )) 
      generate_summary(file) # for download
    })
  
  
#   # MORRIS chart (interactive rchart) - but is too slow
#   output$massPlot <- renderChart2({
#     iso_file <- isoread(file.path(root_dir, "MAT25391411_ON OFFS_linearity.dxf"), type = "CFLOW", read_mass_data = TRUE)
#     
#     m1 <- mPlot(x="time", y=c("mass44", "mass45", "mass46"), data = 
#                   mutate(iso_file$massData, time = round(time), mass44 = round(mass44), mass45 = round(mass45), mass46 = round(mass46)), 
#                 type="Line", parseTime=FALSE)
#     m1$set(pointSize = 0, lineWidth = 1)
#     #m1$set(height=500 width = 500) # works
#     return(m1)
#   })

})