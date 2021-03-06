# create local user library path (not present by default)
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)

packageVerification <- c("shiny")

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

options(shiny.port = 5000)
options(shiny.host  = '127.0.0.1')

path <- getwd()
path <- paste(path, "/core", sep = "")

if(!dir.exists("./shared/"))
    dir.create("./shared/")
if(!dir.exists("./saved/"))
    dir.create("./saved/")

assign("flagStop", FALSE, envir=.GlobalEnv,inherits = FALSE)

browseURL("http://127.0.0.1:5000/")
returnData = runApp(appDir = path)
if(length(returnData) != 0 && returnData$goto == 1)
{
    path <- getwd()
    path <- paste(path, "/irace_scenario_setup/app.R", sep = "")
    print('Loading setup section...')
    source(path)
}
if(length(returnData) != 0 && returnData$goto == 2)
{
    path <- getwd()
    path <- paste(path, "/reports/app.R", sep = "")
    assign("loadedCustomSection", TRUE, envir=.GlobalEnv, inherits = FALSE)
    assign("pathRDATA", returnData$path, envir=.GlobalEnv, inherits = FALSE)
    assign("recentlyLoadedReports", FALSE, envir=.GlobalEnv,inherits = FALSE)
    print("Loading reports section...")
    source(path)
}