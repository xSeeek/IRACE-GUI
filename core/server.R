server <- function(input, output, session) {
    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader
        status <- list(goto = 2, path = dataToLoad$datapath)

        assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
        session$sendCustomMessage(type = "closeWindow", message = "message")
        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    observeEvent(input$launchSetup, {
        status <- list(goto = 1)

        assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
        session$sendCustomMessage(type = "closeWindow", message = "message")
        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    session$onSessionEnded(function() {
        if(flagStop == FALSE)
        {
            print('SESSION ENDED BY MAIN APP')
            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            session$sendCustomMessage(type = "closeWindow", message = "message")
            stopApp()
        }
    })
}