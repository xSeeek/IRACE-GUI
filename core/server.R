server <- function(input, output, session) {
    observeEvent(input$reportLoader, {
        if(flagStop == FALSE)
        {
            dataToLoad <- input$reportLoader

            if(length(dataToLoad$type) != 1 && dataToLoad$type != 'application/x-r-data')
            {
                session$sendCustomMessage(type = "invalidFiletype", message = "message")
                return(NULL)
            }

            status <- list(goto = 2, path = dataToLoad$datapath)

            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            session$sendCustomMessage(type = "closeWindow", message = "message")
            stopApp(returnValue = invisible(status))
        }
    }, once = FALSE)

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
