packageVerification <- c("shiny", "irace", "magick", "shinythemes", "shinydashboard", "DT", "RCurl")

local({r <- getOption("repos")
    r["CRAN"] <- "http://cran.us.r-project.org" 
    options(repos=r)
})

pkgCheck <- function(packages)
{
    for(package in packages)
    {
        # if package is installed locally, load
        if(package %in% rownames(installed.packages()))
            do.call('library', list(package))

        # if package is not installed locally, download, then load
        else 
        {
            install.packages(package)
            do.call("library", list(package))
        }
    } 
}

pkgCheck(packageVerification)

options(shiny.port = 5003)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/core", sep = "")

if(length(ls(envir=.GlobalEnv, pattern="iraceResults")) == 0)
{
    if(length(ls(envir=.GlobalEnv, pattern="pathRDATA")) != 0)
        load(pathRDATA, envir=.GlobalEnv)
    else
        load(paste0(getwd(), '/resources/data/iraceResults.Rdata'), envir=.GlobalEnv)
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
