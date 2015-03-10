#' Utility functions for the data viewer

# DATA ===============

#' Load isodat files
#' @param files file paths
#' @param progress callback function, will be called with filename and total number of files as parameters
load_isodat_files <- function(files, progress = NULL) {
  iso_files <- list()
  for (file in files) {
    if (!is.null(progress)) progress(basename(file), length(files))
    iso_files <- c(iso_files, isoread(file, type = "CFLOW"))
  }
  names(iso_files) <- sapply(iso_files, function(i) i$filename)
  return(iso_files)
}

#' Combine isodat files' data tables
#' @param isodat_files isodat objects
get_isodat_data_tables <- function(isodat_files, select = names(isodat_files[[1]]$get_data_table())) {
  do.call(rbind, lapply(isodat_files, function(i) {
    mutate(i$get_data_table()[select], file = i$filename, date = i$creation_date)
  }))
}

#' get a subset of the data table and prepare it for plotting by assigning x and y
#' @param data_table the whole data table
#' @param pattern regexp pattern to find which files to include
#' @param x name of the x column
#' @param y name of the y column
get_plot_data_table <- function(data_table, pattern, x, y) {
  df <- subset(data_table, grepl(pattern, file))
  df$x <- df[[x]]
  df$y <- df[[y]]
  return(df)
}

#' Combine isodat files' mass traces
#' @param isodat_files isodat objects
get_isodat_mass_traces <- function(isodat_files) {
  do.call(rbind, lapply(isodat_files, function(i) mutate(i$get_mass_data(melt = T)[c("time", "signal", "variable")], file = i$filename)))
}

# PLOTTING ===========

#' make isodat file mass plot
#' @param mass_traces the combined mass traces from get_mass_traces()
#' @param file filename of the one to plot
plot_masses <- function(mass_traces, files, masses) {
  if (length(files) > 0 && files[1] != "0") {
    message("Showing traces ", paste(masses, collapse=", "), " for ", paste(files, collapse = ", "))
    isolate({
      ggplot(
        subset(mass_traces, variable %in% masses & file %in% files),
        aes(time, signal, linetype = variable, colour = file)) +
        geom_line() +
        scale_x_continuous(expand = c(0,0)) + 
        labs(x = "Time [s]", y = "Signal [mV]", linetype = "Trace", colour = "File") +
        theme_bw() + theme(
          legend.position = "bottom", legend.direction = "vertical",
          text = element_text(size = 18)) +
        guides(
          color = guide_legend(ncol = 2, byrow = TRUE, title.hjust = 0.5),
          linetype = guide_legend(nrow = 1, title.hjust = 0.5)
          )
    })
  } else
    plot.new()
}

# GENERIC UI ELEMENTS ===========

#' make a trace selector box
make_trace_selector <- function(id, isodat_files, label = "Traces", size = 4) {
  selectInput(
    id, label, 
    if (length(isodat_files) > 0) names(isodat_files[[1]]$plotOptions$masses) else c("Nothing loaded yet"="0"),
    selected = if (length(isodat_files) > 0) names(isodat_files[[1]]$plotOptions$masses) else c(),
    multiple=TRUE, selectize=FALSE, size = 4)
}

#' make a file selector box
make_file_selector <- function(id, isodat_files, label = "Files", selected = NULL, size = 7) {
  files <- names(isodat_files)
  if (is.null(selected) || length(selected <- grep("linearity", files, value = T)) == 0)
    selected <- files[1] # if none selected, select first one
  selectInput(
    id, label, 
    if (length(files) > 0) files else c("Nothing loaded yet"="0"),
    selected = selected, multiple=TRUE, selectize=FALSE, size = size)
}
