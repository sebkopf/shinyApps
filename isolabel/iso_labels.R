# labeling strengths
get_iso_labels <- reactive({
  df <- data$iso_labels %>% 
    subset(!error & include == "yes") 
  if (nrow(df) == 0) return(df)
  else df  %>% mutate(
    Label = paste0("Dil=", vol, ":", input$label.ref_vol, ", Conc=", conc, ", ", input$ref, "=", spike, "at%"))
})

# hide/show iso labels edit box
observe({
  if (length(input$iso_labels_rows_selected) > 0)  {
    
    if (!identical(data$iso_label_selected, input$iso_labels_rows_selected)) {
      with(data$iso_labels[input$iso_labels_rows_selected, ], {
        updateTextInput(session, "iso_label_edit_vol", value = vol)
        updateTextInput(session, "iso_label_edit_conc", value = conc)
        updateSliderInput(session, "iso_label_edit_spike", value = spike)
        updateCheckboxInput(session, "iso_label_edit_include", value = include == "yes")
      })
    }
    
    shinyjs::show("iso_labels_editbox_div")
  } else
    shinyjs::hide("iso_labels_editbox_div")
  
  # update selection tracker (this is important so updating the table doesn't cause a loop)
  data$iso_label_selected <- input$iso_labels_rows_selected
})

# save iso label entry (could combine into one 'observer' but then updating record unnecessarily frequently)
update_iso_label_column <- function(value, var) {
  data$iso_labels[isolate(input$iso_labels_rows_selected), var] <- value
}
observe(input$iso_label_edit_vol %>% update_iso_label_column("vol"))
observe(input$iso_label_edit_conc %>% update_iso_label_column("conc"))
observe(input$iso_label_edit_spike %>% update_iso_label_column("spike"))
observe( (if(input$iso_label_edit_include) "yes" else "no") %>% update_iso_label_column("include"))

# isolabels table
output$iso_labels <- DT::renderDataTable({
  if (data$starting) return(NULL) # not initialized yet
  spike_label <- paste0("spike [at% ", data$nat@isoname, "]")
  effective_label <- paste0("effective [at% ", data$nat@isoname, "]")
  
  data$iso_labels %>% 
    dplyr::select(vol, conc, spike, effective = effective.wab, include) %>% 
    dplyr::mutate(effective = effective %>% get_value("percent") %>% signif(3)) %>% 
    dplyr::rename_(.dots = list("spike", "effective") %>% setNames(c(spike_label, effective_label))) %>% 
  datatable(
    rownames = FALSE, filter = 'none',  selection = "single",
    #extensions = 'Responsive', # unfortunately does not work in shiny apps yet (should keep the sizing better)
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = 0:4)), # center cols
      pageLength = nrow(data$iso_labels), autoWidth = TRUE, dom = "t", ordering = FALSE,
      initComplete = JS(
        "function(settings, json) {", # reselect same row after reloading
          if (!is.null(isolate(input$iso_labels_rows_selected)))
            paste0(
              "$('#iso_labels div table').DataTable().$('tr:eq(",
              isolate(input$iso_labels_rows_selected) - 1,
              ")').addClass('selected');"),
        "}")
      )
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