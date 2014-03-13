library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Isotope labeling calculator"),
    
    # Sidebar with a slider input for number of observations
    sidebarPanel(
        selectInput("ref", "Choose an isotope system:", 
                    choices = c("2H", "13C", "15N", "18O", "34S")),
        sliderInput("label1", 
                    "Isotope label strengths:", 
                    min = 0,
                    max = 1,
                    step = 0.001,
                    value = 0.01, 
                    format="#.#%"),
        sliderInput("label2", 
                    "", 
                    min = 0,
                    max = 1,
                    step = 0.001,
                    value = 0.05, 
                    format="#.#%"),
        sliderInput("label3", 
                    "", 
                    min = 0,
                    max = 1,
                    step = 0.001,
                    value = 0.15, 
                    format="#.#%"),
        checkboxGroupInput(inputId="show", label="Show labeleling strengths:", choices=1:3, selected=1:3),
        tags$style(type="text/css", HTML("#show>*{float: left; margin-right: 15px; height: 20px;} #show {height: 20px;}")),
        
        selectInput(
            "targetType", "Target enrichment:",
            c(`Relative enrichment [in permil]` = "permil",
              `Total fractional abundance [% light isotope]` = "frac"), selected="permil"),
        
        conditionalPanel(
            condition = "input.targetType == 'permil'",
            sliderInput("intensity_permil", 
                        "", 
                        min = 100,
                        max = 50000,
                        step = 100,
                        value = 500, 
                        format="#.#permil",
                        animate=TRUE)
        ),
        
        conditionalPanel(
            condition = "input.targetType == 'frac'",
            sliderInput("intensity_F", 
                        "", 
                        min = 0,
                        max = 1,
                        step = 0.001,
                        value = 0.01, 
                        format="#.#%",
                        animate=TRUE)
        )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("plot")
    )
))