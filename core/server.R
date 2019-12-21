server <- function(input, output, session) {
    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader
        status <- list(goto = 2, path = dataToLoad$datapath)

        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    session$onSessionEnded(function() {
        stopApp()
    })
}