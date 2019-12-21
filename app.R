library('shiny')
options(shiny.port = 5000)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/core", sep = "")

browseURL("http://127.0.0.1:5000/")
returnData = runApp(appDir = path)
if(length(returnData) != 0 && returnData$goto == 1)
{
    print('To Setup section')
}
if(length(returnData) != 0 && returnData$goto == 2)
{
    path <- getwd()
    path <- paste(path, "/reports/app.R", sep = "")
    assign("loadedCustomSection", TRUE, envir=.GlobalEnv, inherits = FALSE)
    assign("pathRDATA", returnData$path, envir=.GlobalEnv, inherits = FALSE)
    print("Loading reports section...")
    source(path)
}