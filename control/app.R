
        

packageVerification <- c("shiny", "irace","readr","magick","RCurl","shinydashboard","devtools")

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

load(paste0(getwd(), '/resources/test-dummy/acotsp-arena/irace.Rdata'), envir=.GlobalEnv)
    
browseURL("http://127.0.0.1:5002/")
runApp(appDir = path)