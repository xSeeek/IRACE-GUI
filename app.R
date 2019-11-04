packageVerification <- c("shiny", "irace")

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

path <- getwd()
path <- paste(path, "/core", sep = "")

runApp(appDir = path)