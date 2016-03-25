library(shiny)
source("folder_browser.R")
shinyUI(
  fluidPage(
        
        h1("Folder selector demo"),
        h4("Folder selector output: "),
        uiOutput("readout"),
        hr(),
        
        # actual folder selector part
        folderSelectorInput("fs1")
        
  )
)



