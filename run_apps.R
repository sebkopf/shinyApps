library(shiny)
run_app <- function(name, ...) {
    library(shiny)
    runApp(name, ...)
}

isolabel <- function() run_app("isolabel")
dynamic <- function() run_app("dynamic")
linearity <- function() run_app("linearity", host = "0.0.0.0", port = 1234)