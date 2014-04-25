library(shiny)

shinyServer(function(input, output, session) {
    output$dyn_input <- renderUI({
        inputs <- lapply(1:input$n, function(i) {
            input_name  <- paste("Test", i, sep="")
            input_score <- paste("Score", i, sep="")
            choices <- c("Fall NWF"="FallNWF",
                         "Fall ORF"="FallORF",
                         "Spring NWF"="SpringNWF",
                         "Spring ORF"="SpringORF")
            wellPanel(      
                selectInput(
                    input=  input_name,
                    label= "Test Name", choices=choices, selected = choices[i]),
                sliderInput(input_score,"Score:",
                            min=0, max=250,value=0))
        })
        do.call(tagList, inputs)
    }) 
})