library(shiny)
library(isotopia)
library(lubridate)
library(stringr)
library(ggplot2)
library(grid)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(magrittr)

source("calculations.R")

shinyServer(function(input, output, session) {
    
    # Delayed rendering --------------
    session$onFlushed(function() {
        data$starting <- FALSE
    })
    
    # Data fields --------------
    data <- reactiveValues(
      starting = TRUE,
      
      gen_times = data.frame(
        value = c(1, 1, 1, 1, 10),
        units = c("hour", "day", "month", "year", "year"),
        include = "yes",
        stringsAsFactors = FALSE
      ),
      
      iso_labels = data.frame(
        vol = c(1, 5, 10), 
        conc = c(1, 1, 1), 
        spike = c(50, 50, 99),
        include = "yes",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      iso_label_selected = NULL,
      
      nat = NA,
      target.ab = NA,
      target.delta = NA,
      
      plot1.df = NULL,
      plot2.df = NULL,
      table.df = NULL
    )
    
    # Generation times ----------------------------------------------
    source("gen_times.R", local = TRUE)
    
    
    # Isotope labels ------------------------------------------------
    source("iso_labels.R", local = TRUE)
    
    
    # Main inputs processing ------------------------------------------------
    observe({
      
      # find target
      data$nat <- get_standard(minor = input$ref)
      if (input$targetType == 'permil')
        target <- delta(input$intensity_permil, ref_ratio = data$nat)
      else 
        target <- abundance(input$intensity_F/100)
      target <- set_attrib(target, minor = data$nat@isoname, major = data$nat@major) # set isotope names
      data$target.ab <- to_ab(to_ratio(target)) # abundance value
      data$target.delta <- to_delta(to_ratio(target), ref_ratio = data$nat) # delta value
      
      # find spikes via weighted abundance (wab) 
      init.wab <- abundance(data$nat %>% to_ab() %>% get_value() %>% rep(nrow(data$iso_labels)), 
                            weight = input$label.ref_vol * input$label.ref_conc)
      
      # assign updated iso_labels data frame
      data$iso_labels <-
        data$iso_labels %>% 
        mutate(
          spike.wab = abundance(spike/100, weight = vol * conc),
          effective.wab = (spike.wab + init.wab) %>% set_attrib(minor = data$nat@isoname, major = data$nat@major),
          error = effective.wab <= data$target.ab
        )
    })
    
    # Main plot/table data generation ------------------------------------------------
    observe(
      withProgress(session, min=1, max=5, {
        setProgress(message = 'Calculating ...')
        setProgress(value = 2)
        
        input$updatePlot1 # update button pressed
        input$updatePlot2 # update button pressed
        input$updateTable # update button pressed
        input$tableDataType # simple notation trigger also instantaneous
        input$plot2DataType # simple notation trigger also instantaneous
        
        isolate({
          data$plot1.df <- NULL
          data$plot2.df <- NULL
          data$table.df <- NULL
          
          # checks
          if (data$target.ab <= to_ab(data$nat)) return() # problem
          if (nrow(get_iso_labels()) == 0) return() # no label is strong enough or excluded because of error
          if (nrow(get_doubling_times()) == 0) return() # no doubling times selected
          
          # plot 1 data (labeling times)
          data$plot1.df <- 
            label_time(
              dblt = get_doubling_times()$dblt, 
              target = data$target.ab, 
              tracer = get_iso_labels()$effective.wab,
              natural = data$nat) %>% 
            merge(get_iso_labels(), by.x = "tracer.ab", by.y = "effective.wab") %>% 
            mutate(Label = factor(Label, levels = get_iso_labels()$Label))
          setProgress(value = 3)
          
          # plot 2 data (enrichment curves)
          times <- seq(0, max(data$plot1.df$labeling_time) * input$plot2Xzoom/100, length.out = 50)
          data$plot2.df <- 
            label_strength(
              time = c(1, times), 
              dblt = get_doubling_times()$dblt, 
              tracer = get_iso_labels()$effective.wab,
              natural = data$nat) %>% 
            merge(get_doubling_times(), by = "dblt") %>% 
            merge(get_iso_labels(), by.x = "tracer.ab", by.y = "effective.wab") %>% 
            mutate(
              DBLT = factor(label, levels = get_doubling_times()$label),
              Spike = factor(Label, levels = get_iso_labels()$Label),
              enrichment = get_value(if(input$plot2DataType == "permil") total.delta else total.ab)
            )
          
          setProgress(value = 4)
          
          # table 1 data
          data$table.df <- 
            get_iso_labels()$effective.wab %>% 
            # calculate the labeling curves for each tracer
            lapply(function(i){
              df <- subset(data$plot1.df, tracer.ab == i)
              label_strength(
                time = df$labeling_time, 
                dblt = df$dblt, 
                tracer = abundance(i) %>% set_attrib(minor = data$nat@isoname, major = data$nat@major),
                natural = data$nat)
            }) %>% 
            dplyr::bind_rows() %>% 
            # merge in doubling times and iso labels
            dplyr::left_join(get_doubling_times() %>% mutate(label = label %>% paste("doubling")), by = "dblt") %>% 
            dplyr::left_join(get_iso_labels(), by.x = "tracer.ab", by.y = "effective.wab") %>% 
            mutate(
              `Incubation time [s]` = as.numeric(time), 
              `Incubation time` = time.label,
              enrichment = 
                if (input$tableDataType == "permil") {
                  total.delta %>% get_value() %>% signif(3) %>% paste()
                } else {
                  total.ab %>% get_value() %>% multiply_by(100) %>% signif(3) %>% paste()
                }) %>% 
            # cast into wide format
            dcast(`Label` + `Incubation time [s]` + `Incubation time` ~ label, value.var = "enrichment")
          
          print(data$table.df)
          
          # header order
          header_order <- match(paste(get_doubling_times()$label, "doubling"), names(data$table.df))
          data$table.df <- data$table.df[c(1,3,header_order)]
          setProgress(value = 5)
        })
      }))
    
    
    # Plots ------------------------------------------------
    source("plots.R", local = TRUE)
    
    # hide/show plot settings box
    observe({
      if (input$tabs == "plot2")  {
        shinyjs::show("plot_settings_div")
      } else
        shinyjs::hide("plot_settings_div")
    })
    
    # Table ------------------------------------------------
    source("table.R", local = TRUE)
    
    # Info Outputs ---------------------------------------------
    
    # spikes table header
    output$rare_iso_header <- renderUI({
      ref <- get_standard(minor = input$ref)
      list(strong("Initial composition:", 
             ref %>% to_ab() %>% switch_notation("percent") %>% get_value() %>%  signif(3),
             "at%", input$ref))
    })
    
    # natural abundance info output
    output$nat <- renderUI({
        ref <- get_standard(minor = input$ref)
        paste0("Reference: ", get_label(ref), ": ", signif(get_value(ref), 4))
    })
    
    # labeling strength problems
    output$iso_labels_error <- renderUI({
      if (nrow(data$iso_labels %>% subset(!error & include == "yes")) == 0) {
        return (paste0("Error: please include at least one valid labeling strength."))
      } else if (any(data$iso_labels$error)) {
          return (paste0("Warning: some cannot possibly reach target enrichment."))
      } else
        return ("")
    })
    
    # enrichment info output
    output$intensity_F_message <- renderUI({ 
        if (data$target.ab <= to_ab(data$nat))
            return(paste0("Error: target enrichment too small, it must at least exceed natural abundance."))
        else
            return ("")
    })

})
