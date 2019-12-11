library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(irace, lib.loc = "/usr/local/lib/R/site-library")
library(magick)
      
  
packageVerification <- c("shiny", "irace","readr","magick","RCurl")


local({r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org" 
options(repos=r)
})

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