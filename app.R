library('shiny')
options(shiny.port = 5000)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/core", sep = "")

browseURL("http://127.0.0.1:5000/")
returnData = runApp(appDir = path)
path <- getwd()
path <- paste(path, "/reports/app.R", sep = "")
assign("loadedCustomSection", TRUE, envir=.GlobalEnv, inherits = FALSE)
assign("pathRDATA", returnData, envir=.GlobalEnv, inherits = FALSE)
print("Loading reports section...")
source(path)