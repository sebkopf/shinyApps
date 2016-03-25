folderSelectorInput <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("path")),
    fluidRow(
      column(width = 6, uiOutput(ns("folder"))),
      column(width = 6, uiOutput(ns("files")))
    ),
    
    # reset sub folder selection when switching tabs
    tags$script(sprintf(
      " $('#%s').on('click', function(){
        Shiny.onInputChange('%s', '');
        })", 
      ns("path"), ns("new_folder"))),
    
    # enable sub folder selection on double click
    tags$script(sprintf(
      " $('#%s').on('dblclick', function(){
        var obj = $('select#%s')[0];
        Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
        })", 
      ns("folder"), ns("folder"), ns("new_folder")))
  )
}

folderSelector <- function(input, output, session, root, 
                           folders_label = "Select folder:",
                           folders_sort_desc = FALSE,
                           files_label = "Files in selected folder:",
                           files_sort_desc = FALSE,
                           files_pattern = "\\.", size = 8) {
  
  # namespace
  ns <- session$ns
  
  # base folder
  base_dir <- reactive({
    
    # base path
    path <- input$path
    if (is.null(input$path)) {
      path <- root 
    }
    
    # sub folder selection
    if (!is.null(input$new_folder) && input$new_folder != "" && dirname(input$new_folder) == path) {
      path <- input$new_folder
    }
    
    # safety checks
    stopifnot(file.exists(path))
    stopifnot(R.utils::isAbsolutePath(path))
    return(path)
  })
  
  # base folder sub directories
  base_sub_dirs <- reactive({
    sub_dirs <- setNames(
      list.dirs(base_dir(), recursive = F, full.names = T), 
      list.dirs(base_dir(), recursive = F, full.names = F))
    if (folders_sort_desc)
      sub_dirs <- rev(sub_dirs)
    return(sub_dirs)
  })
  
  # selected sub directory
  selected_sub_dir <- reactive({
    sub_dir <- input$folder
    if (is.null(sub_dir) && length(base_sub_dirs()) > 0) {
      sub_dir <- base_sub_dirs()[1]
    } else if (is.null(sub_dir)) {
      sub_dir <- base_dir()
    }
    return(sub_dir)
  })
  
  # generate path tabs
  output$path <- renderUI({
    tmp_path <- base_dir()
    parents <- list(id = ns("path"), selected = tmp_path)
    while (tmp_path != dirname(root)){
      parents <- c(parents, list(tabPanel(basename(tmp_path), value = tmp_path)))
      tmp_path <- dirname(tmp_path)
    }
    do.call(tabsetPanel, args = parents[length(parents):1])
  })
  
  # generate folders listing
  output$folder <- renderUI({
    selectInput(ns("folder"), folders_label, 
                size = size, selectize = F, width = "100%",
                base_sub_dirs())
  })
  
  # generate content listing
  output$files <- renderUI({
    file_options <- list.files(selected_sub_dir(), pattern = files_pattern)
    if (files_sort_desc)
      file_options <- rev(file_options)
    selectInput(ns("files"), files_label, 
                size = size, selectize = F, multiple = T, width = "100%",
                file_options)
  })
  
  return(selected_sub_dir)
}
