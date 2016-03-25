library(shiny)
source("folder_browser.R")
shinyServer(
  function(input, output, session) {
    
    # folder selector part
    fs1_folder <- callModule(folderSelector, "fs1", root = dirname(getwd()),
                             files_pattern = "\\..+", size = 4,
                             files_label = "Files:")
    
    fs2_folder <- callModule(folderSelector, "fs2", 
                             root = dirname(getwd()),
                             folders_label = "Select a folder:",
                             folders_sort_desc = TRUE,
                             files_label = "Folder content:",
                             files_sort_desc = FALSE)
    
    # output
    output$readout1 <- renderUI({
      if (input[["fs1-trigger"]] > 0) {
        return(isolate(fs1_folder()))
      } 
      return("dialog not launched yet")
    })
    output$readout2 <- renderUI(fs2_folder())

  }
  
)