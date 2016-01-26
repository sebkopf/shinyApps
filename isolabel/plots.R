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
