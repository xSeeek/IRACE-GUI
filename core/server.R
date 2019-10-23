absolutePath <- getwd()

server <- function(input, output) {
    resourcesPath <- paste(absolutePath, "/IRACE-Interface/resources/", sep = "")
    print(resourcesPath)
}