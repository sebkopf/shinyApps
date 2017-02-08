library(shiny)
library(shinyBS)
source("file_browser.R")
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
        folderSelectorInput("fs2"),
        
        
        h1("File selector demo"),
        
        
        h4("Modal file selector: "),
        modalFileSelectorInput(id = "file1",
                               open_label = "Select a file", dialog_label = "File dialog", close_label = "Go",
                               link_wrapper = h4, allow_upload = FALSE),
        uiOutput("file1_readout"),
        hr(),
        
        h4("In frame file selector:"),
        fileSelectorInput("file2", allow_upload = TRUE, upload_label = "Upload"),
        htmlOutput("file2_readout"),
        hr()
  )
)



