library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(irace)
options(shiny.port = 5000)
  

packageVerification <- c("shiny", "irace", "assertthat", "plotly","DT","ggplot2")

pkgCheck <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!library(x,character.only = TRUE)) 
      stop("Package not found")
  }
}
path <- getwd()
path <- paste(path, "/ProyectoIRACE/IRACE-GUI/Proyecto", sep = "")


runApp(appDir = path)