library(shiny)
library(DT)
library(readxl)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(latex2exp)
library(magrittr)
library(plyr)
library(reshape2)

# PARAMETERS and PROCESSES =======
params <<- read_excel("nox_isotope_dynamics.xlsx", sheet = "variables") %>% 
  mutate(mjax_latex = paste0("\\(", gsub("\\\\\\\\","\\\\", latex),"\\)"))
value_ids <- (params %>% subset(adjustable == "yes"))$id
plist <<- subset(params) %>% dlply(.(id), identity)
iframe <<- subset(params, type %in% c("var", "inst", "const")) %>% mutate(i=0) %>% dcast(i~id, value.var = "default")
procs <<- read_excel("nox_isotope_dynamics.xlsx", sheet = "processes")
if (is.null(procs$name)) stop("process names not defined")
if (is.null(procs$call)) stop("process calls not defined")
source("nox_isotope_processes.R") # source this

#FIXME:
# there is a bug when increasing the number of steps to 15 and plotting things against each other (e.g. D(15,18) vs. [NO3])
# path doesn't seem to be in right order --> make sure to fix that

server <- function(input, output, session) {

  # get model output
  get_model_data <- reactive({
    input$refresh # refresh pressed
    input$refresh2
    
    values <- isolate(data$values[c("id", "value")])
    
    # FIXME: maybe scenarios is obsolete?
    scenarios <- list(
      list(name = "Flux 1", args = list(D_conc = subset(values, id == "mass_flux")$value))
    )
    
    # FIXME: there might be a more elegant way to do this?
    for (i in 1:nrow(values)) {
      if (values$id[i] %in% names(iframe) && !is.null(values$value[i])) {
          iframe[1,values$id[i]] <- values$value[i]
      }
    }
    
    procs <- data$processes[isolate(input$processes_rows_selected),]
    if (nrow(procs) == 0) return(NULL)
    
    df <- run_processes(procs, scenarios, iframe, steps = isolate(input$model_steps))
    df.cross <- merge(df, df, by = c("process", "scenario", "i")) 
    return(df.cross)
  })
  
  # get data for plot
  get_plot_data <- reactive({
    df.cross <- get_model_data()
    if (is.null(df.cross)) return (NULL)

    xs <- isolate(data$axes[input$axis_x_rows_selected, "id"])
    ys <- isolate(data$axes[input$axis_y_rows_selected, "id"])
    if (length(xs) == 0 || length(ys) == 0 || is.null(procs)) return(NULL)

    plot.df <- df.cross %>% 
      subset(variable.x %in% xs & variable.y %in% ys) %>% 
      ddply(.(process, scenario, variable.x, variable.y), mutate, 
            undefined = all(is.na(value.x)) || all(is.na(value.y)),
            unchanged = identical(value.x[2], value.x[max(i)]) & identical(value.y[2], value.y[max(i)])) %>%
      subset(!undefined & !unchanged) # don't display arrows for unchanged values (compare to i=2 b/c w/o starting value, NA@1)
    
    plot.df %>% subset(scenario == "Flux 1") %>% return()
  })

  # main plot
  make_main_plot <- reactive(
    withProgress(message = 'Rendering plot...', value = 0.2, {
      setProgress(detail = 'Calculating isotopic effects ...', value = 0.3)
      data <- get_plot_data()
      setProgress(detail = 'Generating graph ...', value = 0.5)
      if (is.null(data) || nrow(data) == 0)
        ggplot() + theme_bw() + annotate("text", label = "please select your axes\nand relevant processes\nand refresh the plot", x = 0.5, y = 0.5, size = 12) + 
          labs(x = "", y = "") %>% return()
      else {
        p <- .base_plot 
        if (isolate(input$legend) == "below")
          p <- p + theme(legend.position = "bottom") + guides(color = guide_legend(ncol=2,byrow=FALSE)) 
        
        #if (isolate(input$layout) == "wrap")
        #  data[order(data$i),] %>% plot_in_wrap(plot = p) %>% return()
        #else
        data[order(data$i),] %>% plot_in_grid(plot = p) %>% return() 
      }
    })
  )
  
  
  ### parameters ###
  data <- reactiveValues(
    values = params %>% subset(adjustable == "yes") %>% mutate(parameter = mjax_latex, description = name, value = default),
    processes = procs,
    axes = params %>% subset(type %in% c("var", "inst"))
  )
  
  # edit entry
  output$editbox_name <- renderText({
    if (length(input$parameters_rows_selected) > 0) {
      paste0(
        data$values[input$parameters_rows_selected,"description"], " [",
        data$values[input$parameters_rows_selected,"unit"], "]:")
    } else {
      return("")
    }
  })
  
  # hide/show edit box
  observe({
    if (length(input$parameters_rows_selected) > 0)  {
      updateTextInput(session, "editbox_value", 
                      value = data$values[input$parameters_rows_selected,"value"])
      shinyjs::show("editbox_div")
    } else
      shinyjs::hide("editbox_div")
  })
  
  # save entry
  observe({
    if (is.na(input$editbox_value)) return()
    isolate({
      # update data here
      data$values[input$parameters_rows_selected,"value"] <- input$editbox_value
      # update display data.table
      shinyjs::runjs(
        paste0(
          "var table = $('#DataTables_Table_0').DataTable();",
          "table.cell('.selected', 3).data(", input$editbox_value, ").draw();")
      )
    })
  })
  
  # parameter table
  output$parameters <- DT::renderDataTable({
    datatable(isolate(data$values[c("parameter", "type", "description", "value", "unit")]), 
              rownames = FALSE, filter = 'none',  class = 'cell-border stripe', selection = "single", escape = FALSE,
              #extensions = 'TableTools',
              options = list(pageLength = isolate(nrow(data$values)), autoWidth = TRUE, searchHighlight = TRUE, dom = 'frt') 
              #               tableTools = list(sSwfPath = copySWF()), # doesn't fully work but look into it again
              )
  }, server = FALSE)
  
  # processes
  output$processes <- DT::renderDataTable({
    datatable(data$processes[,"name", drop=FALSE], 
              rownames = FALSE, filter = 'none',  selection = "multiple",
              options = list(pageLength = nrow(data$processes), autoWidth = TRUE, dom = "t", ordering = FALSE)
    )
  }, server = FALSE)
  
  # axes
  output$axis_x <- DT::renderDataTable({
    datatable( mutate(data$axes, `X-axis` = mjax_latex)[, "X-axis", drop = FALSE], 
              rownames = FALSE, filter = 'none',  selection = "multiple", escape = FALSE,
              options = list(pageLength = nrow(data$axes), autoWidth = TRUE, dom = "t", ordering = FALSE)
    )
  }, server = FALSE)
  output$axis_y <- DT::renderDataTable({
    datatable( mutate(data$axes, `Y-axis` = mjax_latex)[, "Y-axis", drop = FALSE], 
               rownames = FALSE, filter = 'none',  selection = "multiple", escape = FALSE,
               options = list(pageLength = nrow(data$axes), autoWidth = TRUE, dom = "t", ordering = FALSE)
    )
  }, server = FALSE)
  
  # rendering plot
  output$main_plot <- renderPlot(
    make_main_plot(), 
    height = reactive({input$refresh; input$refresh2; isolate(input$main_plot_height)})) # trigger also only on refresh
  
  # download handler
  output$save <- downloadHandler(
    filename = function() { isolate(input$save_name) }, 
    content = function(file) { 
      device <- function(..., version="1.4") grDevices::pdf(..., version=version)
      ggsave(file = file, plot = make_main_plot(), width = isolate(input$save_width), height = isolate(input$save_height), device = device)
    })
}
