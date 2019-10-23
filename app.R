library(shiny)
library(shinythemes)
options(shiny.port = 5000)

path <- getwd()
path <- paste(path, "/ProyectoIRACE/IRACE-GUI/Proyecto", sep = "")


runApp(appDir = path)