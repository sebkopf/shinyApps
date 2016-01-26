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
          "var table = $('#DataTables_Table_1').DataTable();",
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
