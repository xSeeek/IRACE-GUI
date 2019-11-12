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


summary <- function(input,output){
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()
  
  output$elites <- DT::renderDataTable({
    req(input$iterationsElites)
    allElitesID <- iraceResults$allElites
    for(i in allElitesID[as.integer(input$iterationsElites)])
    {
      bestConfiguration <- getConfigurationById(iraceResults, ids=i)
      print(bestConfiguration)
      print(i)
    }
    DT::datatable(bestConfiguration)
  })

  output$numIterations <- renderText(
    iraceResults$state$nbIterations
  )
  output$numConfigurations <- renderText(
    conf <- length(iraceResults$allConfigurations$.ID.)
  )
  output$numInstances <- renderText(
    instances <- length(iraceResults$state$.irace$instancesList$instance)
  )
  output$numElitesConfigurations <- renderText(
    for(i in iraceResults$allElites)
    {
      c(i)
      return(length(i))
    }
  )
  
  output$dataTableAllConfigurations <- DT::renderDataTable(
    iraceResults$allConfigurations,
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      pageLength = 5
    )
  )
  
  output$plotPerformance <- renderPlot({
    req(input$iterationPerformance)
    fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
    elites <- as.character(iraceResults$iterationElites)
    values <- colMeans(iraceResults$testing$experiments[,elites])
    plot(fes,values,type="s",xlab="Number of runs of the target algorithm",ylab= "Mean value over testing set")
    points(fes,values)
    text(fes,values,elites,pos=1)
  })
  
  output$frecuencyParameters <- renderPlot({
    req(input$iterationFrequency)
    conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = c(input$iterationFrequency[1],input$iterationFrequency[2]))
    parameterFrequency(conf, iraceResults$parameters)
  })
  
    output$paralelCoordinatesCandidates <- renderPlot({
    req(input$iterationPC)
    last <- length(iraceResults$iterationElites)
    conf <- getConfigurationByIteration(iraceResults = iraceResults,iterations = c(input$iterationPC[1],input$iterationPC[2]))
    parallelCoordinatesPlot(conf, iraceResults$parameters, param_names = c("algorithm", "alpha","beta","rho","q0"), hierarchy = FALSE)
  })
    
    output$boxPlotBestConfiguration <- renderPlot({
      req(input$iterationBoxPlot)
      results <- iraceResults$experiments[,iraceResults$allElites[[input$iterationBoxPlot]]]
      conf <- gl(ncol(results),
                 nrow(results),
                 labels = colnames(results)
              )
      pairwise.wilcox.test(as.vector(results), conf,paired = TRUE, p.adj ="bonf")
      configurationsBoxplot(results, ylab = "Solution Cost")
    })
    
      output$performance <- renderPlot({
        iters <- unique(iraceResults$experimentLog[,"iteration"])
        fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
        elites <- as.character(iraceResults$iterationElites)
        values <- colMeans(iraceResults$experiments[,elites])
        plot(fes,
             values,
             type = "s",
             xlab = "Number of runs of the target algorithm",
             ylab = "Mean value over testing set"
        )
        points(fes,values)
        text(fes, values, elites, pos = 1)
        
      })
}