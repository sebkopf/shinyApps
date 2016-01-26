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
    values <- reactiveValues(starting = TRUE)
    session$onFlushed(function() {
        values$starting <- FALSE
    })
    
    # Generation times --------------
    
    data <- reactiveValues(
      gen_times = data.frame(
        value = c(1, 1, 1, 1, 10),
        units = c("hour", "day", "month", "year", "year"),
        include = "yes",
        stringsAsFactors = FALSE
      )
    )
    
    # hide/show gen times edit box
    observe({
      if (length(input$gen_times_rows_selected) > 0)  {
        with(data$gen_times[input$gen_times_rows_selected, ], {
          updateTextInput(session, "dblt_edit_value", value = value)
          updateSelectInput(session, "dblt_edit_units", selected = units)
          updateCheckboxInput(session, "dblt_edit_include", value = include == "yes")
        })
        shinyjs::show("gen_editbox_div")
      } else
        shinyjs::hide("gen_editbox_div")
    })
    
    # save gen times entry
    observe({
      if (is.na(input$dblt_edit_value) || is.na(input$dblt_edit_units) || is.na(input$dblt_edit_include)) return()
      isolate({
        data$gen_times[input$gen_times_rows_selected,"value"] <- input$dblt_edit_value
        data$gen_times[input$gen_times_rows_selected,"units"] <- input$dblt_edit_units
        data$gen_times[input$gen_times_rows_selected,"include"] <- if (input$dblt_edit_include) "yes" else "no"
        with(data$gen_times[input$gen_times_rows_selected, ], {
          shinyjs::runjs(
            paste0(
              "var table = $('#DataTables_Table_0').DataTable();",
              "table.cell('.selected', 0).data(", value, ").draw();",
              "table.cell('.selected', 1).data('", units, "').draw();",
              "table.cell('.selected', 2).data('", include, "').draw();")
          )
        })
      })
    })
    
    # gen times table
    output$gen_times <- DT::renderDataTable({
      datatable(isolate(data$gen_times), 
                rownames = FALSE, filter = 'none',  selection = "single",
                options = list(
                  columnDefs = list(list(className = 'dt-center', targets = 0:2)), # center cols
                  pageLength = nrow(isolate(data$gen_times)), autoWidth = TRUE, dom = "t", ordering = FALSE)
      )
    }, server = FALSE)
    
    
    # Inputs processing ------------------------------------------------
    
    # doubling times
    get_doubling_times <- reactive({
      data$gen_times %>% 
        dplyr::filter(include == "yes") %>% 
        dplyr::distinct() %>% 
        dplyr::group_by_(.dots = names(data$gen_times)) %>% 
        dplyr::mutate(
          dblt = duration(as.integer(value), units), 
          label = paste0(value, " ", units, if(value > 1) "s"))
    })
    
    # labeling strengths
    labelsInput <- reactive({
        data <- list()
        
        # natural abundance reference 
        data$nat <- get_standard(minor = input$ref)
        
        # target enrichment
        if (input$targetType == 'permil')
            data$target <- delta(input$intensity_permil, ref_ratio = data$nat)
        else 
            data$target <- abundance(input$intensity_F)
        
        data$target <- set_attrib(data$target, minor = data$nat@isoname, major = data$nat@major) # set isotope names
        data$target.ab <- to_ab(to_ratio(data$target)) # abundance value
        data$target.delta <- to_delta(to_ratio(data$target), ref_ratio = data$nat) # delta value
        
        # weighted natural abundance
        wnat <- abundance(rep(get_value(data$nat), 3), weight = input$label.ref_vol * input$label.ref_conc)
        
        # weighted spikes
        data$strengths <- sapply(1:3, function(i) input[[paste0("label", i)]], simplify = T)
        data$vols <- sapply(1:3, function(i) input[[paste0("label", i, "_vol")]], simplify = T)
        data$concs <- sapply(1:3, function(i) input[[paste0("label", i, "_conc")]], simplify = T)
        wspikes <- abundance(data$strengths, weight = data$vols * data$concs)
        
        # spike sum
        data$spikes <- set_attrib(abundance(get_value(wnat + wspikes)), minor = data$nat@isoname, major = data$nat@major)
        data$spikes.show <- 1:3 %in% as.integer(input$show)
        data$spikes.error <- (data$spikes <= data$target.ab)
        
        return(data)
    })
    
    # MAIN  input processeing
    datasetInput <- reactive({
        # triggers for actually (re)calculating the data set
        input$tabs # reload when new tab is selected
        input$updatePlot1 # update button pressed
        input$updatePlot2 # update button pressed
        input$updateTable # update button pressed
        
        # Most of this heavy lifting for doing the calculations is isolated so that it only gets run triggered when any of 
        # the above changes (none of the settings on the left)
        isolate({
            dblts <- get_doubling_times()
            data <- labelsInput()
            
            # labels
            show <- (which(data$spikes.show & !data$spikes.error))
            
            # errors
            if (length(show) == 0)
                stop("Please enable at least one valid labeling strength.")
            if (data$target.ab <= to_ab(data$nat))
                stop("Please choose a higher target enrichment.")
            
            # spikes data frame
            spikes.df <- mutate(data.frame(data[c("spikes", "vols", "concs", "strengths")]),
                             Label = paste0("Dil=", vols, ":", input$label.ref_vol, ", Conc=", concs, ", ", input$ref, "=", 100*strengths, "at%"))
            
            # labeling times for plot
            plot.df <- label_time(
                dblt = dblts$dblt, 
                target = data$target.ab, 
                spike = data$spikes[show],
                natural = data$nat)
            plot.df <- merge(plot.df, spikes.df, by.x = "spike", by.y = "spikes")
            plot.df$Label <- factor(plot.df$Label, levels = spikes.df$Label) # legend
            
            # data table for enrichment curves
            plot2.df <- label_strength(
                time = duration(c(1, seq(0, max(plot.df$labeling_time) * input$plot2Xzoom/100, length.out = 50)), "seconds"), 
                dblt = dblts$dblt, 
                spike = data$spikes[show],
                natural = data$nat)
            
            # merge with user defined dblts and define factors and reporting format depending on request
            plot2.df <- merge(plot2.df, dblts[c("dblt", "label")], by = 'dblt')
            plot2.df <- merge(plot2.df, spikes.df, by.x = "spike.ab", by.y = "spikes")
            plot2.df <- mutate(plot2.df,
                DBLT = factor(label, levels = dblts$label),
                Spike = factor(Label, levels = spikes.df$Label),
                enrichment = get_value(if(input$plot2DataType == "permil") total.delta else total.ab))
            
            # enrichment for table
            table.df <- data.frame()
            for (i in 1:length(data$spikes[show])) {
                ispike <- data$spikes[show][i]
                df <- subset(plot.df, spike.ab == ispike)
                table.df <- rbind(table.df, 
                  label_strength(
                    time = duration(df$labeling_time, "seconds"), 
                    dblt = duration(df$dblt, "seconds"), 
                    spike = to_abundance(ispike),
                    natural = data$nat)
                )
            }
            
            # merge with labels and spikes
            table.df <- merge(table.df, mutate(dblts, label = paste(label, "doubling"))[c("dblt", "label")], by = 'dblt')
            table.df <- merge(table.df, spikes.df[c("spikes", "Label")], by.x = "spike.ab", by.y = "spikes")
            
            # adjust headers
            table.df <- mutate(
                table.df, 
                `Incubation time [s]` = as.numeric(time), 
                `Incubation time` = time.label)
            
            if (input$tableDataType == "permil") {
                table.df$enrichment <- signif(get_value(table.df$total.delta), 2) 
            } else {
                table.df$enrichment <- paste0(signif(get_value(table.df$total.ab, "percent"), 2)) 
            }
            
            # cast 
            table.df <- dcast(table.df, `Label` + `Incubation time [s]` + `Incubation time` ~ label, value.var = "enrichment" )
            header_order <- match(paste(dblts$label, "doubling"), names(table.df))
            table.df <- table.df[c(1,3,header_order)]
            
            return(c(
                dblts, data, list(plot.df = plot.df, plot2.df = plot2.df, table.df = table.df)))
        })
    })
    
    # Plots ------------------
    
    # hide/show edit box
    observe({
      if (input$tabs == "plot2")  {
        shinyjs::show("plot_settings_div")
      } else
        shinyjs::hide("plot_settings_div")
    })
    
    # plot1
    plotInput <- reactive({
        data <- datasetInput() 
        
        # only update if datasetInput changed
        p <- isolate({
          ggplot(data$plot.df,
                 aes(x = dblt, y = labeling_time, fill = Label, colour = Label)) + 
            geom_segment(aes(x=0, xend=dblt, y=labeling_time, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), ends="first", type="open", 
                                     angle=15), linetype=2) + 
            geom_segment(aes(x=dblt, xend=dblt, y=0, yend=labeling_time), 
                         arrow=arrow(length=unit(0.4,"cm"), type="open", angle=15), 
                         linetype=2) + 
            geom_line(linetype=1) + geom_point(colour="black", shape = 21, size=4) +
            scale_y_log10("labeling time",
                          expand = c(0, 0.1), breaks = unique(data$plot.df$labeling_time), 
                          labels = unique(data$plot.df$labeling_time.label)) + 
            scale_x_log10("generation time", expand = c(0, 0.2), breaks = data$dblt, 
                          labels = data$label) + 
            scale_fill_manual("Isotope spike", values = brewer.pal(3, "Set1")) +
            scale_color_manual("Isotope spike", values = brewer.pal(3, "Set1")) +
            labs(title = paste0("Required labeling times for enrichment to:\n", 
                                get_label(data$target.ab), " = ", signif(get_value(data$target.ab, "percent"), 3), " at% / ",
                                get_name(data$target.delta), " = ", round(get_value(data$target.delta)), " ", isotopia:::get_units(data$target.delta))) + 
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title.align = 0.5,
                  text = element_text(size = 16)) +
            guides(fill = guide_legend(title.position = "top", nrow = 3)) 
        })
        if (isolate(input$legend) == "below")
          p <- p + theme(legend.position = "bottom") + guides(color = guide_legend(ncol=2,byrow=FALSE)) 
        return(p)
    })
    
    # plot
    plot2Input <- reactive({
        data <- datasetInput()
        
        # only update if datasetInput changed
        p <- isolate({
            if (input$plot2DataType == "permil") {
                ylab <- get_label(data$plot2.df$total.delta)
                ylabeller <- function(x) format(x, big.mark = ",", scientific = FALSE)
            } else {
                ylab <- get_label(data$plot2.df$total.ab)
                ylabeller <- function(x)  paste0(signif(100*x, 1), " at%")
            }
          
            ggplot(data$plot2.df,
                   aes(x = as.numeric(time), y = enrichment, colour = DBLT)) + 
                geom_line(linetype=1) + 
                scale_x_continuous("", breaks = function(limits) pretty(limits, 10), labels = duration_label) +
                scale_y_continuous(ylab, label = ylabeller) +
                scale_colour_manual("Generation time", values = brewer.pal(5, "Set1")) + 
                facet_grid(~Spike) + 
                theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                theme(
                    text = element_text(size = 16),
                    legend.title.align = 0.5,
                    axis.text.x = element_text(angle = 60, hjust = 1)) +
                coord_cartesian(
                   xlim = c(min(as.numeric(data$plot2.df$time)), max(as.numeric(data$plot2.df$time))),
                   ylim = c(min(data$plot2.df$enrichment), input$plot2Yzoom/100*max(data$plot2.df$enrichment)))
        })
        if (isolate(input$legend) == "below")
          p <- p + theme(legend.position = "bottom") + guides(color = guide_legend(ncol=2,byrow=FALSE)) 
        return(p)
    })
    
    
    
    # Outputs rendering ---------------------------------------------
    
    # spikes table header
    output$rare_iso_header <- renderUI({
        paste0(input$ref, " [at%]")
    })
    
    # natural abundance info output
    output$nat <- renderUI({
        ref <- get_standard(minor = input$ref)
        paste0("Reference: ", get_label(ref), ": ", signif(get_value(ref), 4))
    })
    
    # labeling strengths messages output
    generate_label_error <- function(i) {
        renderUI({
            data <- labelsInput()
            if (data$spikes.error[i]) {
                return (paste0("Too small, can't possibly reach target enrichment."))
            } else
                return ("")
        })
    }
    generate_label_msg <- function(i) {
        renderUI({
            data <- labelsInput()
            if (data$spikes.error[i]) {
                return("")
            } else {
                d <- to_delta(to_ratio(data$spikes[i]), ref_ratio = data$nat)
                return (paste0("Effective labeling strength: ", signif(get_value(data$spikes[i], "percent"), 3), " at% ", 
                               data$spikes@isoname, " / ", signif(get_value(d), 3) , " permil"))
            }
        })
    }
    
    output$label1_error <- generate_label_error(1)
    output$label2_error <- generate_label_error(2)
    output$label3_error <- generate_label_error(3)   
    output$label1_msg <- generate_label_msg(1)
    output$label2_msg <- generate_label_msg(2)
    output$label3_msg <- generate_label_msg(3) 
    
    # enrichment info output
    output$intensity_F_message <- renderUI({ 
        data <- labelsInput()
        if (data$target.ab <= to_ab(data$nat))
            return(paste0("Too small, target enrichment must exceed natural abundance."))
        else
            return ("")
    })
    
   
    # rendering plot
    output$plot <- renderPlot({
      
        # don't load right away (only after the controls on the left have loaded)
        if (values$starting)
            return(NULL)
        
        # don't load unless the right tab is selected
        if (input$tabs != "plot1")
            return(NULL)
        
        withProgress(session, min=1, max=5, {
            setProgress(message = 'Rendering plot ...')
            setProgress(value = 2)
            p <- plotInput()
            setProgress(value = 4)
            suppressWarnings(print(p))
            setProgress(value = 5)
        })
    },
    height = reactive({input$updatePlot1; isolate(input$main_plot_height)}))
    
    # rendering plot2
    output$plot2 <- renderPlot({
        # don't load unless the right tab is selected
        if (input$tabs != "plot2")
            return(NULL)
        
        withProgress(session, min=1, max=5, {
            setProgress(message = 'Rendering plot ...')
            setProgress(value = 2)
            p <- plot2Input()
            setProgress(value = 4)
            suppressWarnings(print(p))
            setProgress(value = 5)
        })
    },
    height = reactive({input$updatePlot2; isolate(input$main_plot_height)}))
    
    # saving plots
    output$save <- downloadHandler(
      filename = function() { isolate(input$save_name) }, 
      content = function(file) { 
        device <- function(..., version="1.4") grDevices::pdf(..., version=version)
        ggsave(file = file, plot = plotInput(), 
               width = isolate(input$save_width), height = isolate(input$save_height), device = device)
      })
    
    output$save2 <- downloadHandler(
      filename = function() { isolate(input$save_name2) }, 
      content = function(file) { 
        device <- function(..., version="1.4") grDevices::pdf(..., version=version)
        ggsave(file = file, plot = plot2Input(), 
               width = isolate(input$save_width2), height = isolate(input$save_height2), device = device)
      })
    
    # rendering table
    output$table <- renderTable({
        # don't load unless the right tab is selected
        if (input$tabs != "table")
            return(NULL)
        
        withProgress(session, min=1, max=5, {    
            setProgress(message = 'Rendering table ...')
            setProgress(value = 2)
            data.table <- datasetInput()$table.df
            setProgress(value = 4)
            return(data.table)
        })
    }, digits = 0, include.rownames = F, align = rep('r', ncol(datasetInput()$table.df) + 1))
    
    # saving table 
    output$save3 <- downloadHandler(
        filename = function() { isolate(input$save_name3) },
        content = function(file) { write.csv(datasetInput()$table.df, file = file) }
    )
    
})
