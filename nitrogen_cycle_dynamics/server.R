library(shiny)
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
params <<- read_excel("nox_isotope_dynamics.xlsx", sheet = "variables")
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

server <- function(input, output) {

  # get model output
  get_model_data <- reactive({
    input$refresh # refresh pressed
    
    values <- isolate(sapply(value_ids, function(var) input[[paste0("value_", var)]]))
    
    print(values)
    
    scenarios <- list(
      #list(name = "Flux 1", args = list(D_conc = get_p("D1_conc"))),
      #list(name = "Flux 2", args = list(D_conc = get_p("D2_conc"))),
      #list(name = "Flux 3", args = list(D_conc = get_p("D3_conc")))
      list(name = "Flux 1", args = list(D_conc = as.numeric(isolate(input$mass_flux))))
    )
    
    for (name in names(values)) {
      if (name %in% names(iframe) && !is.null(values[name])) {
        iframe[1,name] <- values[name]
      }
    }
    
    df <- run_processes(
      procs %>% subset(name %in% isolate(input$procs)), 
      scenarios, iframe, steps = isolate(input$model_steps))
    df.cross <- merge(df, df, by = c("process", "scenario", "i")) 
    return(df.cross)
  })
  
  # get data for plot
  get_plot_data <- reactive({
    df.cross <- get_model_data()
    if (is.null(df.cross)) return (NULL)

    xs <- isolate(input$x_axis)
    ys <- isolate(input$y_axis)
    if (is.null(xs) || is.null(ys) || is.null(procs)) return(NULL)

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
  
  
  
  
  
  # rendering plot
  output$main_plot <- renderPlot(
    make_main_plot(), 
    height = reactive({input$refresh; isolate(input$main_plot_height)})) # trigger also only on refresh
  
  # download handler
  output$save <- downloadHandler(
    filename = function() { isolate(input$save_name) }, 
    content = function(file) { 
      device <- function(..., version="1.4") grDevices::pdf(..., version=version)
      ggsave(file = file, plot = make_main_plot(), width = isolate(input$save_width), height = isolate(input$save_height), device = device)
    })
}
