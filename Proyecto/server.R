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

formatColData <- function(resultsData, iterationData)
{
  vectorColNames <- colnames(resultsData)
  formatedData <- Reduce(intersect, list(vectorColNames, iterationData))
  return(formatedData)
}

elites <- function(input,output){
  allElites <- iraceResults$allElites
  last <- iraceResults$iterationElites
  for(i in allElites)
  {
    bestConfiguration <- getConfigurationById(iraceResults, ids=i)
    print(bestConfiguration)
    return(bestConfiguration)
  }
}
summary <- function(input,output){
  iterations <- iraceResults$state$nbIterations
  count <- 0
    
  output$dataTableElites <- DT::renderDataTable({
    elites()
  })
  
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
    
    
    output$boxPlotPerfomance <- renderPlot({
      req(input$iterationPlotsPerfomance)
      configurationPerIteration <- convertVectorToString(iraceResults$allElites[as.integer(input$iterationPlotsPerfomance)][[1]])
      results <- iraceResults$testing$experiments
      intersectedColumns <- formatColData(results, configurationPerIteration)
      results <- subset(iraceResults$testing$experiments, select=(intersectedColumns))
      conf <- gl(ncol(results), nrow(results), labels = colnames(results))
      pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
      configurationsBoxplot (results, ylab = "Solution cost")
    })
    
    output$boxPlotBestConfiguration <- renderPlot({
      last <- length(iraceResults$iterationElites)
      id <- paste0(iraceResults$iterationElites[last])
      results <- subset(iraceResults$testing$experiments, select=c(id))
      conf <- gl(ncol(results), nrow(results), labels = colnames(results))
      pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
      configurationsBoxplot (results, ylab = "Solution cost")
    })
}