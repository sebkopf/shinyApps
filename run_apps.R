library(shiny)
run_app <- function(name, ...) {
    library(shiny)
    runApp(name, ...)
}

isolabel <- function() run_app("isolabel")
dynamic <- function() run_app("dynamic")
sdv <- function() run_app("sigman_data_viewer", host = "0.0.0.0", port = 1234)