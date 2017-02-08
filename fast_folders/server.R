library(shiny)
source("file_browser.R")
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
    
    # file selector part
    
    files1 <- callModule(fileSelector, "file1", root = dirname(getwd()),
                         pattern = "\\.",
                         root_name = "Modal data",
                         size = 6,
                         sort_desc = FALSE,
                         multiple = FALSE,
                         enable_recent = TRUE,
                         start_recent = FALSE,
                         number_recent = 5)
    
    files2 <- callModule(fileSelector, "file2", root = dirname(getwd()), 
                         pattern = "\\.",
                         root_name = "Data", 
                         size = 10, 
                         sort_desc = TRUE, 
                         multiple = TRUE,
                         enable_recent = TRUE,
                         start_recent = TRUE,
                         number_recent = 10)
    

    # output (SK: this could be more elegant - similar as solved in the fileSelector)
    output$readout1 <- renderUI({
      if (input[["fs1-trigger"]] > 0) {
        return(isolate(fs1_folder()))
      } 
      return("dialog not launched yet")
    })
    output$readout2 <- renderUI(fs2_folder())

    
    # output from modal dialog
    output$file1_readout <- renderUI({
      # if want to only update when modal is closed
      files1$modal_closed()
      isolate(files1$selection_relative()) # isolate is important
    })

    output$file2_readout <- renderUI({
      # for immediate update (both for modal and non-modal)
      HTML(files2$selection_relative() %>% paste(collapse = "<br>"))
    })
    
  }
  
)