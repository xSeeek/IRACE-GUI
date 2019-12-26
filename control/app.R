library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(irace)
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


options(shiny.port = 5000)
options(shiny.host  = '127.0.0.1')
setPath <- setwd("~/ProyectoIRACE/IRACE-GUI")
path <- getwd()
path <- paste(path, "/control/Proyecto", sep = "")

load(paste0(getwd(), '/control/resources/test-dummy/acotsp-arena/irace.Rdata'), envir=.GlobalEnv)
    

runApp(appDir = path)