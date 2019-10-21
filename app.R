library(shiny)
library(shinythemes)
options(shiny.port = 5000)

path <- getwd()
path <- paste(path, "/IRACE-Interface/core", sep = "")

runApp(appDir = path)