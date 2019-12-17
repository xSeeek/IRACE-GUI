packageVerification <- c("shiny", "irace", "safer", "magick", "shinythemes", "shinydashboard", "DT", "RCurl")

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

options(shiny.port = 5003)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/reports/core", sep = "")

if(length(ls(envir=.GlobalEnv, pattern="iraceResults")) == 0)
{
    if(length(ls(envir=.GlobalEnv, pattern="pathRDATA")) != 0)
        load(pathRDATA, envir=.GlobalEnv)
    else
        load(paste0(getwd(), '/reports/resources/data/iraceResults.Rdata'), envir=.GlobalEnv)
}
        

repeat{
    browseURL("http://127.0.0.1:5003/")
    returnData = runApp(appDir = path)
    if(length(returnData) != 0)
    {
        rm(iraceResults, envir = .GlobalEnv)
        load(returnData, envir=.GlobalEnv)
        assign("loadedCustomSection", TRUE, envir=.GlobalEnv,inherits = FALSE)
        print("Loaded file within app, restarting...")
    }
}