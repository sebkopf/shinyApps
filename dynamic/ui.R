library(shiny)

# Define UI that plots the isotope label enrichment 
shinyUI(
    fluidPage(
        
        # Application headers
        titlePanel("Dynamic test"),
        
        hr(),
        
        sidebarLayout(
            sidebarPanel(
                selectInput("n", "Number of Test Scores Available", choices = c(1,2,3,4), selected = 1),
                uiOutput("dyn_input")
            ),
            mainPanel(h2("in progress..."))
        )
    )
)