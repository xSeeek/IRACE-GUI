absolutePath <- getwd()

server <- function(input, output, session) {
    resourcesPath <- paste(absolutePath, "/resources", sep = "")

    output$iterationSelected <- renderText({
        req(input$iteration)
        paste0('Input: ', input$iteration)
    })
}