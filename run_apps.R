library(shiny)
run_app <- function(name) {
    library(shiny)
    runApp(name)
}

isolabel <- function() run_app("isolabel")
dynamic <- function() run_app("dynamic")