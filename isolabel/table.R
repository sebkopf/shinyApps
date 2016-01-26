# rendering table
output$table <- renderTable({
  # don't load unless the right tab is selected
  if (isolate(input$tabs) != "table" || is.null(data$table.df))
    return(NULL)
  
  withProgress(session, min=1, max=5, {    
    setProgress(message = 'Rendering table ...')
    setProgress(value = 2)
    setProgress(value = 4)
    return(data$table.df)
  })
}, digits = 0, include.rownames = F)

# saving table 
output$save3 <- downloadHandler(
  filename = function() { isolate(input$save_name3) },
  content = function(file) { write.csv(data$table.df, file = file) }
)