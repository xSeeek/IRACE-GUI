server <- function(input, output, session) {
    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader

        stopApp(returnValue = invisible(dataToLoad$datapath))
    }, once = TRUE)
}