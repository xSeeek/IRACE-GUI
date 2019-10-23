absolutePath <- getwd()

#loadData <- function(){
  #parameters <- readParameters(iraceResults, digits = 4, debugLevel = 0,text)
  #parameters
# }
  
server <- function(input, output) {
  resourcesPath <- paste(absolutePath, "/ProyectoIRACE/IRACE-GUI/resources/", sep = "")
  print(resourcesPath)
  #addResourcePath('resources', resourcesPath)
}
