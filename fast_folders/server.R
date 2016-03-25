library(shiny)
source("folder_browser.R")
shinyServer(
  function(input, output, session) {
    
    # folder selector part
    fs1_folder <- callModule(folderSelector, "fs1", root = dirname(getwd()))
    
    # output
    output$readout <- renderUI(fs1_folder())

  }
  
)