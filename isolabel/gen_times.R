# doubling times
get_doubling_times <- reactive({
  dblt <- data$gen_times %>% 
    dplyr::filter(include == "yes") %>% 
    dplyr::distinct() %>% 
    dplyr::group_by_(.dots = names(data$gen_times)) %>% 
    dplyr::mutate(
      dblt = as.numeric(duration(as.integer(value), units)), 
      label = ifelse(value > 1, paste0(value, " ", units, "s"), paste0(value, " ", units))
    )
  return(dblt)
})

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

# save gen times entry - this is an example of updating the table in JS, rather than letting the server do it
# see iso_labels for an example where the server is doing all the updating
update_gen_time_column <- function(value, var, table_col) {
  isolate({
    data$gen_times[input$gen_times_rows_selected, var] <- value
    shinyjs::runjs(
      paste0("$('#gen_times div table').DataTable().",
      "cell('.selected', ", table_col, ").",
      "data('", value, "').draw();")
    )
  })
}

observe(input$dblt_edit_value %>% update_gen_time_column("value", 0))
observe(input$dblt_edit_units %>% update_gen_time_column("units", 1))
observe( (if (input$dblt_edit_include) "yes" else "no") %>% update_gen_time_column("include", 2))


# gen times table
output$gen_times <- DT::renderDataTable({
  datatable(isolate(data$gen_times), # isolated here because we want only JS to update the data table, not server
            rownames = FALSE, filter = 'none',  selection = "single",
            options = list(
              columnDefs = list(list(className = 'dt-center', targets = 0:2)), # center cols
              pageLength = nrow(isolate(data$gen_times)), autoWidth = TRUE, dom = "t", ordering = FALSE)
  )
}, server = FALSE)
