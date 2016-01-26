# labeling strengths
get_iso_labels <- reactive({
  df <- data$iso_labels %>% 
    subset(!error & include == "yes") 
  if (nrow(df) == 0) return(df)
  else df  %>% mutate(
    Label = paste0("Dil=", vol, ":", input$label.ref_vol, ", Conc=", conc, ", ", input$ref, "=", spike, "at%"))
})

# reference, iso target and label strengths
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


# isolabels table
output$iso_labels <- DT::renderDataTable({
  if (data$starting) return(NULL) # not initialized yet
  spike_label <- paste0("spike [at% ", data$nat@isoname, "]")
  effective_label <- paste0("effective [at% ", data$nat@isoname, "]")
  datatable(data$iso_labels %>% 
              dplyr::select(vol, conc, spike, effective = effective.wab, include) %>% 
              dplyr::mutate(effective = effective %>% get_value("percent") %>% signif(3)) %>% 
              dplyr::rename_(.dots = list("spike", "effective") %>% setNames(c(spike_label, effective_label))), 
            rownames = FALSE, filter = 'none',  selection = "single",
            options = list(
              columnDefs = list(list(className = 'dt-center', targets = 0:4)), # center cols
              pageLength = nrow(data$iso_labels), autoWidth = TRUE, dom = "t", ordering = FALSE)
  ) %>% 
    # highlight effective tracers that are not high enough for the target enrichment
    formatStyle(
      effective_label,
      color = styleInterval(data$target.ab %>% get_value("percent") %>% signif(3), c('red', 'black'))
    ) %>% 
    formatStyle(
      "include",
      fontWeight = styleEqual(c("yes", "no"), c("bold", "normal"))
    )
}, server = FALSE)