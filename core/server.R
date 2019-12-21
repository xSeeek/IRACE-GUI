server <- function(input, output, session) {
    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader
        status <- list(goto = 2, path = dataToLoad$datapath)

        assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    session$onSessionEnded(function() {
        if(flagStop == FALSE)
        {
            print('CALLED SESSION ENDED')
            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            stopApp()
        }
    })
}