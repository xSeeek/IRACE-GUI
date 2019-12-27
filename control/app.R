
packageVerification <- c("shiny", "irace","readr","magick","RCurl","shinydashboard","devtools", "shinyjs")

if(!require("dashboardthemes"))
{
  devtools::install_github("nik01010/dashboardthemes")
}

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

          
options(shiny.port = 5002)
options(shiny.host  = '127.0.0.1')
setwd('./control')
path <- getwd()
path <- paste(path, "/Proyecto", sep = "")
load(pathRDATA, envir=.GlobalEnv)
    
browseURL("http://127.0.0.1:5002/")
returnData = runApp(appDir = path)
if(length(returnData) != 0 && returnData$goto == 1)
{
  setwd('../')
  path <- paste(getwd(), "/reports/app.R", sep = "")
  print(path)
  assign("loadedCustomSection", TRUE, envir=.GlobalEnv, inherits = FALSE)
  assign("recentlyLoadedReports", FALSE, envir=.GlobalEnv,inherits = FALSE)
  print("Loading reports section...")
  source(path)
}