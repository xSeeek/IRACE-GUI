absolutePath <- getwd()

server <- function(input, output) {
  resourcesPath <- paste(absolutePath, "../resources", sep = "")
  print(resourcesPath)
}

Plot1 <- function(input, output){
  output$newBoxPlot <- renderPlot({
    configuracionID <- iraceResults$allElites[[1]]
    configuraciones <- iraceResults$allElites$experiments[[1]]
    boxplot(configuraciones,configuracionID,xlab="Configurations ID",ylab="Configurations")
  })
}
