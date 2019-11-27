packageVerification <- c("shiny", "irace", "safer")

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

path <- getwd()
path <- paste(path, "/core", sep = "")

if(length(ls(envir=.GlobalEnv, pattern="iraceResults")) == 0)
  load('resources/data/iraceResults.Rdata', envir=.GlobalEnv)

repeat{
    browseURL("http://127.0.0.1:5000/")
    returnData = runApp(appDir = path)
    if(length(returnData) != 0)
    {
        rm(iraceResults, envir = .GlobalEnv)
        load(returnData, envir=.GlobalEnv)
        assign("loadedCustomSection", TRUE, envir=.GlobalEnv,inherits = FALSE)
        print("Loaded file within app, restarting...")
    }
}
