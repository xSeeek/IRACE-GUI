library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(irace)

server <- function(input, output) {
  resourcesPath <- paste(absolutePath, "../resources", sep = "")
  absolutePath <- getwd()
  
  addResourcePath(prefix = 'resources', directoryPath = '../resources')
  load('../resources/irace.Rdata', envir=.GlobalEnv)
  
  print(resourcesPath)
  
}


convertVectorToString <- function(vector)
{
  newVector = c()
  for(i in 1:length(vector))
    newVector[i] <- paste0(vector[i])
  return(newVector)
}
summary <- function(input,output){
  
  iterations <- iraceResults$state$nbIterations
  count <- 0
      allElites <- length(iraceResults$allElites)
    
    last <- length(iraceResults$iterationElites)
    for(i in 1:last)
    {
      bestConfigurationID <- iraceResults$iterationElites[i]
      bestConfiguration <- getConfigurationById(iraceResults, ids=bestConfigurationID)
      print(bestConfiguration)
      output$dataTableElites <- DT::renderDataTable(
        bestConfiguration
      )
    }
  
  output$numIterations <- renderText(
    iraceResults$state$nbIterations
  )
  output$numConfigurations <- renderText(
    conf <- iraceResults$state$nbConfigurations
  )
  output$numInstances <- renderText(
    instances <- length(iraceResults$state$.irace$instancesList$instance)
  )
  output$numElitesConfigurations <- renderText(
    for(i in 1:length(iraceResults$allElites[[iterations]][iterations]))
    {
      count = count + 1
      return(count)
    }
  )
  
  output$dataTableAllConfigurations <- DT::renderDataTable(
    iraceResults$allConfigurations,
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  #output$getData <- reactive({
   # last <- length(iraceResults$iterationElites)
    #for(i in 1:last)
    #{
      #bestConfigurationID <- iraceResults$iterationElites[i]
      #bestConfiguration <- getConfigurationById(iraceResults, ids=bestConfigurationID)
    #  print(bestConfigurationID)
     # print(allElites)
    #}
    #bestConfiguration
  #})

  
  
  output$frecuencyParameters <- renderPlot({
    parameterFrequency(iraceResults$allConfigurations, iraceResults$parameters)
  })
  
    output$paralelCoordinatesCandidates <- renderPlot({
    req(input$iterationPC)
    last <- length(iraceResults$iterationElites)
    conf <- getConfigurationByIteration(iraceResults = iraceResults,iterations = c(input$iterationPC[1],input$iterationPC[2]))
    parallelCoordinatesPlot(conf, iraceResults$parameters, param_names = c("algorithm", "alpha","beta","rho","q0"), hierarchy = FALSE)
  })
}