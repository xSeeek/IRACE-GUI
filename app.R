library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(irace)

  
  
packageVerification <- c("shiny", "irace","readr")
pkgCheck <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!library(x,character.only = TRUE)) 
      stop("Package not found")
  }
}
  

for (i in 1:length(packageVerification)) 
  pkgCheck(packageVerification[i])

  path <- getwd()
path <- paste(path, "/ProyectoIRACE/IRACE-GUI/Proyecto", sep = "")

options(shiny.port = 5000)
runApp(appDir = path)