library(shiny)
library(shinyBS)
source("folder_browser.R")
shinyUI(
  fluidPage(
        
        h1("Folder selector demo"),
        
        h4("Modal folder selector output: "),
        uiOutput("readout1"),
        hr(),
        
        modalFolderSelectorInput("fs1", size = "small",
                                 dialog_open_label = "Click me", 
                                 dialog_close_label = "Done",
                                 dialog_close_id = "trigger"),
        
        hr(),
        h4("In frame folder selector output: "),
        uiOutput("readout2"),
        hr(),
        
        # actual folder selector part
        folderSelectorInput("fs2")
        
        
        
  )
)



