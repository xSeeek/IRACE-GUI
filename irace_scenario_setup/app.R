packageVerification <- c("shiny", "shinyBS", "devtools", "shinydashboard")

local({r <- getOption("repos")
    r["CRAN"] <- "http://cran.us.r-project.org" 
    options(repos=r)
})

pkgCheck <- function(packages)
{
    for(package in packages)
    {
        if(package %in% rownames(installed.packages()))
            do.call('library', list(package))

        else 
        {
            install.packages(package)
            do.call("library", list(package))
        }
    } 
}

pkgCheck(packageVerification)

options(shiny.port = 5001)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/irace_scenario_setup", sep = "")

browseURL("http://127.0.0.1:5001/")
returnData = runApp(appDir = path)