#----- FILE SELECTOR

library(magrittr)
library(dplyr)

#' fileSelector
#' @param allow_upload whether to allow upload
fileSelectorInput <- function(id, allow_upload = FALSE, upload_label = NULL) {
  ns <- NS(id)
  dialog_tags <-
    tagList(
      
      uiOutput(ns("tab_path")),
      uiOutput(ns("content_list")),
      if (allow_upload) fileInput(ns("upload"), upload_label),
      
      # reset selection when switching tabs
      tags$script(
        "
        $('#%s').on('click', function(){
        Shiny.onInputChange('%s', Math.random());
        })" %>%
        sprintf(ns("tab_path"), ns("tab_click"))),
      
      # enable sub folder selection on double click
      tags$script(
        "
        $('#%s').on('dblclick', function(){
        var obj = $('select#%s')[0];
        Shiny.onInputChange('%s', obj.options[obj.selectedIndex].value);
        Shiny.onInputChange('%s', Math.random());
        })" %>%
        sprintf(ns("content_list"), ns("content_list"),
                ns("content_double_click_item"), ns("content_double_click")))
    )
  
  return(dialog_tags)
  }

#' modalFileSelector
#' @param open_label the label for the link to open the modal dialog
#' @param dialog_label the label of the modal dialog, same as the open_label by default
#' @param close_label the label for the button to close the modal dialog
#' @param ... parameters for fileSelectorInput
modalFileSelectorInput <- function(id, open_label = "Select file", dialog_label = open_label, close_label = "Select", link_wrapper = identity, ...) {
  ns <- NS(id)
  modal_dlg <- bsModal(ns("modal_dialog"), dialog_label, ns("modal_link"), size = "small",
                       column(width = 12, fileSelectorInput(id, ...)) %>% fluidRow())
  
  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$children[[1]] <- close_label
  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$id <- ns("close")
  modal_dlg$children[[1]]$children[[1]]$children[[3]]$children[[1]]$attribs$class <- "btn btn-default action-button"
  
  dialog_tags <-
    tagList(
      link_wrapper(actionLink(ns("modal_link"), open_label, icon = icon("file"))),
      modal_dlg
    )
  return(dialog_tags)
}

#' @param root directory
#' @param root_name the root directory name
#' @param sort_desc whether to sort in descending order
#' @param pattern regexp file selection pattern
#' @param size number of rows in the selection box
fileSelector <- function(input, output, session,
                          root, root_name = basename(root), size = 8, 
                          sort_desc = FALSE, pattern = NULL, multiple = TRUE,
                          enable_recent = TRUE, start_recent = TRUE, number_recent = 10) {
  
  # namespace
  ns <- session$ns
  
  # reactive values
  values <- reactiveValues(
    upload_counter = 1,
    last_double_click_item = "",
    current_directory = root,
    current_directory_content = NULL,
    content_list = c(),
    recent_active = FALSE
  )
  
  # uploads
  observe({
    # upload (expand zip)
    upload <- input$upload
    if (is.null(upload)) return()
    ext <- stringr::str_match(basename(upload$name), "\\.(\\w+$)")[1,2]
    target <- isolate(values$current_directory)
    if (!is.na(ext) && ext == "zip") upload$datapath %>% unzip(exdir = target)
    else upload$datapath %>% file.copy(to = file.path(target, upload$name))
    
    # info and update trigger
    counter <- isolate(values$upload_counter)
    sprintf("Upload #%d complete: %s", counter + 1, file.path(target, upload$name)) %>%
      message()
    values$upload_counter <- counter + 1
  })
  
  # update reactive value last_double_click_item
  observe({
    validate(need(input$content_double_click, message = FALSE)) # only run if set
    input$content_double_click
    values$last_double_click_item <- isolate(input$content_double_click_item)
  })
  
  # update reactive value current_directory
  observe({
    # refresh whenever directory tab is clicked or subfolder is double clicked
    input$tab_click
    input$content_double_click
    
    # isolate everything after
    isolate({
      # base path (start at root if not set yet, unless recent_active parameter is TRUE)
      path <- input$tab_path
      if (is.null(path)) {
        if (start_recent) path <- "<<RECENT>>"
        else path <- root
      } 
      
      # check if recent is selected
      if (path == "<<RECENT>>") {
        if (!values$recent_active) {
          values$recent_active <- TRUE
        } 
        return() # done
      }
      
      # see if last double click item is a valid subdirectory
      subdir <- values$last_double_click_item
      if (!is.null(subdir) && subdir != "" &&
          dirname(subdir) == path && # make sure it's a subdirectory
          dir.exists(subdir) # make sure it's a directory
      ) {
        path <- subdir
        values$last_double_click_item <- "" # reset
      }
      
      # safety checks
      stopifnot(file.exists(path))
      if (!R.utils::isAbsolutePath(path)) stop("not an absolute path: ", path)
      
      # update reactive value
      if (values$recent_active || path != values$current_directory) {
        values$current_directory <- path
        values$current_directory_content <- NULL
        values$recent_active <- FALSE
      }
    })
  })
  
  # update reactive value content_list
  observe({
    
    # triggers if directory changes or if recent is activated
    # triggers when current folder changes (checks for updates every second)
    invalidateLater(1000, session)
    
    if (values$recent_active) {
      # only show recent files (recursive from root)
      new_content <- 
        list.files(root, pattern = pattern, full.names = T, recursive = TRUE) %>% 
        lapply(function(file) list(file = file, mtime = file.mtime(file))) %>% 
        bind_rows() %>% 
        arrange(desc(mtime)) %>% 
        filter(row_number() <= number_recent)
      old_content <- isolate(values$current_directory_content)
      if (!is.null(old_content) && setdiff(new_content$mtime, old_content) %>% length() == 0) 
        return() # okay, nothing changed
      
      # update content list (stores the mtime vector)
      values$current_directory_content <- new_content$mtime
      message("INFO: (Re)loading recent files (NS: ", ns(NULL),")")
      values$content_list <- setNames(new_content$file, 
                                      sub("^[\\/]?", "",
                                      sub(root, "", new_content$file, fixed = TRUE)))
       
    } else {
      # content of selected folder (files and subfolders)
      path <- values$current_directory
      new_content <- c(list.dirs(path, rec = FALSE), list.files(path, pattern = pattern))
      old_content <- isolate(values$current_directory_content)
      if (!is.null(old_content) && setdiff(new_content, old_content) %>% length() == 0) 
        return() # okay, nothing changed
      
      # update contents list (stores the filename vector)
      values$current_directory_content <- new_content
      message("INFO: (Re)loading folder: ", path, " (NS: ", ns(NULL),")")
      folders <- list.dirs(path, rec=FALSE, full.names = T) # w/ full names
      files <- setdiff(list.files(path, full.names = T, pattern = pattern), folders)
      # sort in descending order if asked for
      if (sort_desc) {
        folders <- rev(folders)
        files <- rev(files)
      }
      
      # update content list
      values$content_list <-
        setNames(
          c(folders, files),
          c(sprintf("[ %s ]", folders %>% sapply(basename)), files %>% sapply(basename))
        )
    }
  })
  
  # generate path tabs
  output$tab_path <- renderUI({
    tmp_path <- values$current_directory
    selected_tab <- if (enable_recent && isolate(values$recent_active)) "<<RECENT>>" else tmp_path
    parents <- list(id = ns("tab_path"), selected = selected_tab)
    while (tmp_path != dirname(root)){
      if (tmp_path == root)
        parent <- root_name
      else
        parent <- basename(tmp_path)
      parents <- c(parents, list(tabPanel(parent, value = tmp_path)))
      tmp_path <- dirname(tmp_path)
    }
    if (enable_recent) {
      # make recent bullet
      parents <- c(parents, list(tabPanel("Recent", value = "<<RECENT>>")))
    }
    do.call(tabsetPanel, args = parents[length(parents):1])
  })
  
  # generate folder content listing
  output$content_list <- renderUI({
    selectInput(ns("content_list"), NULL, width = "100%",
                size = size, selectize = F, multiple = multiple,
                values$content_list)
  })
  
  # return both the current path and the selected folder contents
  list(
    modal_closed = reactive(input$close),
    path = reactive(values$current_directory),
    path_relative = reactive(sub("^[\\/]?", "", sub(root, "", values$current_directory, fixed = TRUE))),
    selection = reactive(input$content_list),
    selection_relative = reactive(sub("^[\\/]?", "", sub(root, "", input$content_list, fixed = TRUE))),
    double_click = reactive(values$last_double_click_item)
  )
}



