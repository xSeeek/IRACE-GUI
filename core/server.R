absolutePath <- getwd()

server <- function(input, output) {
    resourcesPath <- paste(absolutePath, "/resources", sep = "")
    print(resourcesPath)
}