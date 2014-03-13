shinyUI(pageWithSidebar(
    headerPanel("My Shiny App"),
    sidebarPanel(),
    mainPanel(
        h1("First level title"),
        h2("Second level title"),
        h3("Third level title"),
        h4("Fourth level title"),
        h5("Fifth level title"),
        h6("Sixth level title")
    )
))