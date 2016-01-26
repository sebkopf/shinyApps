error_plot <- 
  ggplot() + theme_bw() + 
  annotate("text", label = "Please choose valid input parameters.", x = 0.5, y = 0.5, size = 12) + 
  labs(x = "", y = "")
  

# plot1
generate_plot1 <- reactive({
  
  if (is.null(data$plot1.df)) {
    return(error_plot)
  }
  
  p <- 
    data$plot1.df %>% 
    ggplot() +
    aes(x = dblt, y = labeling_time, fill = Label, colour = Label) + 
    geom_segment(aes(x=0, xend=dblt, y=labeling_time, yend=labeling_time), 
                 arrow=arrow(length=unit(0.4,"cm"), ends="first", type="open", angle=15), linetype=2) + 
    geom_segment(aes(x=dblt, xend=dblt, y=0, yend=labeling_time), 
                 arrow=arrow(length=unit(0.4,"cm"), type="open", angle=15), linetype=2) + 
    geom_line(linetype=1) + geom_point(colour="black", shape = 21, size=4) +
    scale_y_log10("labeling time",
                  expand = c(0, 0.1), breaks = unique(data$plot1.df$labeling_time), 
                  labels = unique(data$plot1.df$labeling_time.label)) + 
    scale_x_log10("generation time", expand = c(0, 0.2), 
                  breaks = isolate(get_doubling_times()$dblt), 
                  labels = isolate(get_doubling_times()$label)) + 
    scale_fill_manual("Isotope spike", values = brewer.pal(3, "Set1")) +
    scale_color_manual("Isotope spike", values = brewer.pal(3, "Set1")) +
    labs(title = paste0("Required labeling times for enrichment to:\n", 
                        get_label(isolate(data$target.ab)), " = ", 
                        signif(get_value(isolate(data$target.ab), "percent"), 3), " at% / ",
                        get_name(isolate(data$target.delta)), " = ", 
                        round(get_value(isolate(data$target.delta))), " ", 
                        isotopia:::get_units(isolate(data$target.delta)))) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.title.align = 0.5,
          text = element_text(size = 16)) +
    guides(fill = guide_legend(title.position = "top", nrow = 3)) 
  
  if (isolate(input$legend) == "below")
    p <- p + theme(legend.position = "bottom") + guides(color = guide_legend(ncol=2,byrow=FALSE)) 
  return(p)
})

# rendering plot1
output$plot <- renderPlot({
  
  # don't load right away (only after the controls on the left have loaded)
  if (data$starting)
    return(NULL)
  
  # don't load unless the right tab is selected
  if (isolate(input$tabs) != "plot1")
    return(NULL)
  
  withProgress(session, min=1, max=5, {
    setProgress(message = 'Rendering plot ...')
    setProgress(value = 2)
    p <- generate_plot1()
    setProgress(value = 4)
    suppressWarnings(print(p))
    setProgress(value = 5)
  })
},
height = reactive({input$updatePlot1; isolate(input$main_plot_height)}))

# saving plot 1
output$save <- downloadHandler(
  filename = function() { isolate(input$save_name) }, 
  content = function(file) { 
    device <- function(..., version="1.4") grDevices::pdf(..., version=version)
    ggsave(file = file, plot = generate_plot1(), 
           width = isolate(input$save_width), height = isolate(input$save_height), device = device)
  })

# plot2
generate_plot2 <- reactive({
  
  if (is.null(data$plot2.df)) {
    return(error_plot)
  }
  
  if (isolate(input$plot2DataType) == "permil") {
    ylab <- get_label(data$plot2.df$total.delta)
    ylabeller <- function(x) format(x, big.mark = ",", scientific = FALSE)
  } else {
    ylab <- get_label(data$plot2.df$total.ab)
    ylabeller <- function(x)  paste0(signif(100*x, 1), " at%")
  }
  
  p <- 
    data$plot2.df %>% 
    ggplot()+
    aes(x = as.numeric(time), y = enrichment, colour = DBLT) + 
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
      ylim = c(min(data$plot2.df$enrichment), 
               isolate(input$plot2Yzoom)/100*max(data$plot2.df$enrichment)))
  
  if (isolate(input$legend) == "below")
    p <- p + theme(legend.position = "bottom") + guides(color = guide_legend(ncol=2,byrow=FALSE)) 
  return(p)
})

# rendering plot2
output$plot2 <- renderPlot({
  # don't load unless the right tab is selected
  if (isolate(input$tabs) != "plot2")
    return(NULL)
  
  withProgress(session, min=1, max=5, {
    setProgress(message = 'Rendering plot ...')
    setProgress(value = 2)
    p <- generate_plot2()
    setProgress(value = 4)
    suppressWarnings(print(p))
    setProgress(value = 5)
  })
},
height = reactive({input$updatePlot2; isolate(input$main_plot_height)}))

# saving plot 2
output$save2 <- downloadHandler(
  filename = function() { isolate(input$save_name2) }, 
  content = function(file) { 
    device <- function(..., version="1.4") grDevices::pdf(..., version=version)
    ggsave(file = file, plot = plot2Input(), 
           width = isolate(input$save_width2), height = isolate(input$save_height2), device = device)
  })
