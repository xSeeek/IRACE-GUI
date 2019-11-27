library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(irace)
library(readr)
load('../resources/test-dummy/acotsp-arena/irace.Rdata', envir=.GlobalEnv)

updateFile <- function()
{
  load('../resources/test-dummy/acotsp-arena/irace.Rdata', envir=.GlobalEnv)
  return(file)
}


fileReaderData <- reactiveFileReader(500,
                                     NULL,
                                     filePath = 'resources/test-dummy/acotsp-arena/irace.Rdata', 
                                     read_file(file.path("/home/daser/ProyectoIRACE/IRACE-GUI/resources/test-dummy/acotsp-arena/irace.Rdata"),locale("es",decimal_mark = ",")))

server <- function(input, output) {
  absolutePath <- getwd()
  resourcesPath <- paste(absolutePath, "../resources", sep = "")

  
  addResourcePath(prefix = 'resources', directoryPath = '../resources')
  
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


summary <- function(input,output,session){
  
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()

  
  
  
  
  #### TABLAS ####
  output$elites <- DT::renderDataTable({
    req(input$iterationsElites)
    invalidateLater(5000, session)
    #withProgress(message = "Calculation in progress...",
     #            detail = "This may take a while...",
      #           value = 0, {
       #            for(x in 1:iraceResults$scenario$maxExperiments){
        #             incProgress(1/iraceResults$scenario$maxExperiments)
         #            Sys.sleep(0.25)
          #         }
           #      })
      allElitesID <- iraceResults$allElites
      for(i in allElitesID[as.integer(input$iterationsElites)])
      {
        bestConfiguration <- getConfigurationById(iraceResults, ids=i)
      }
      DT::datatable(bestConfiguration)
  })
  
  
  output$dataTableAllConfigurations <- DT::renderDataTable({
    invalidateLater(5000, session)
    DT::datatable(iraceResults$allConfigurations,
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      pageLength = 5
    ))
  })
  
  #### SUMMARY ####
  
  output$numIterations <- renderText({
    iraceResults$state$nbIterations
  })
  output$numConfigurations <- renderText({
    invalidateLater(5000, session)
    conf <- length(iraceResults$allConfigurations$.ID.)
  })
  output$numInstances <- renderText({
    invalidateLater(5000, session)
    instances <- length(iraceResults$state$.irace$instancesList$instance)
  })
  observe({
    invalidateLater(5000, session)
    output$numElitesConfigurations <- renderText({
      dataUpdate <- updateFile()
      for(i in iraceResults$allElites)
      {
        c(i)
        return(length(i))
      }
    })
  })
 #### PLOTS ####
  
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
    req(input$parametersFrequency)
    
    fixFormat <- iraceResults$parameters
    fixFormat$names <- input$parametersFrequency
    
    conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = c(input$iterationFrequency[1],input$iterationFrequency[2]))
    parameterFrequency(conf, fixFormat)
  })
  
    output$paralelCoordinatesCandidates <- renderPlot({
    req(input$iterationPC)
    req(input$parametersParallelCoordinates)
      
    fixFormat <- iraceResults$parameters
    fixFormat$names <- input$parametersParallelCoordinates
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



  #### PROGRESS BAR ####

