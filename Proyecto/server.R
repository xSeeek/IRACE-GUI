library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(irace, lib.loc = "/usr/local/lib/R/site-library")
library(readr)
library(magick)
absolutePath <- getwd()
load('../resources/irace-gcc.Rdata', envir=.GlobalEnv)
updateFile <- function()
{
  load('../resources/irace-gcc.Rdata', envir=.GlobalEnv)
  return(file)
}


removeTemporalPlots <- function()
{
  junk <- dir(pattern="tempPlot")
  file.remove(junk)
}
generateFrequencyPlot <- function(iterations, parameters)
{
  configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterations[1]:iterations[2])
  
  max <- 12
  limit <- 1
  params <- c()
  numberOfParameters <- ceiling(length(parameters)/max)
  base64plots <- c();
  for(i in 1: numberOfParameters)
  {
    k <- 1
    for(j in limit:(max*i))
    {
      if(length(parameters) >= j)
      {
        params[k] <- parameters[j]
        k <- k + 1
      }
    }
    
    # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
    fixFormat <- iraceResults$parameters
    fixFormat$names <- params
    
    png(filename <- paste0("tempPlotFrequency", i, ".png"), width = 1500, height = 1500, res = 200)
    parameterFrequency(configurations, fixFormat)
    dev.off()
    
    base64image <- base64Encode(readBin(filename, "raw", file.info(filename)[1, "size"]), "txt")
    base64image <- paste0('data:image/png;base64,', base64image)
    base64plots[i] <- base64image
    
    limit <- (max*i) + 1;
  }
  removeTemporalPlots()
  return(base64plots)
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
  
  withProgress(message = 'Showing Data', value = 0,{
    n <- length(iraceResults)
    for(i in 1:n)
    {
      incProgress(1/n, detail = paste("Doing part", i))
    }
  })
  
  
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()

  
  
  
  
  #### TABLAS ####
  output$elites <- DT::renderDataTable({
    req(input$iterationsElites)
    validate(
      need(input$iterationsElites <= iraceResults$state$nbIterations, "Ingrese un valor valido")
    )
      allElitesID <- iraceResults$allElites
      for(i in allElitesID[as.integer(input$iterationsElites)])
      {
        bestConfiguration <- getConfigurationById(iraceResults, ids=i)
      }
      DT::datatable(bestConfiguration,
                    options = list(
                      scrollX = TRUE,
                      scrollY = TRUE
                    ))
  })
  
  
  output$dataTableAllConfigurations <- DT::renderDataTable({
    DT::datatable(iraceResults$allConfigurations,
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      pageLength = 5
    ))
  })
  
  #### SUMMARY ####
  
  output$numOfParameters <- renderText({
    invalidateLater(1000,session)
    length(iraceResults$parameters$names)
  })
  
  
  output$iraceVersion <- renderText({
    invalidateLater(1000,session)
    iraceResults$irace.version
  })
  
  
  output$experimentsUsedSoFar <- renderText({
    invalidateLater(1000,session)
    iraceResults$state$experimentsUsedSoFar
  })
  output$maxExperiments <- renderText({
    invalidateLater(1000, session)
    iraceResults$scenario$maxExperiments
  })
  
  output$numIterations <- renderText({
    invalidateLater(1000, session)
    iraceResults$state$nbIterations
  })
  output$numConfigurations <- renderText({
    invalidateLater(1000, session)
    conf <- length(iraceResults$allConfigurations$.ID.)
  })
  output$numInstances <- renderText({
    invalidateLater(1000, session)
    nrow(iraceResults$experiments)
  })
  observe({
    invalidateLater(1000, session)
    output$numElitesConfigurations <- renderText({
      req(input$iterationForElites)
      for(i in iraceResults$allElites[as.integer(input$iterationForElites)])
      {
        c(i)
        return(length(i))
      }
    })
  })
 #### PLOTS ####
  
  output$plotPerformance <- renderPlot({
    req(input$iterationPerformance)
    invalidateLater(1000, session)
    fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
    elites <- as.character(iraceResults$iterationElites)
    values <- colMeans(iraceResults$testing$experiments[,elites])
    plot(fes,values,type="s",xlab="Number of runs of the target algorithm",ylab= "Mean value over testing set")
    points(fes,values)
    text(fes,values,elites,pos=1)
  })
  
  output$frecuencyParameters <- renderImage({
    req(input$iterationFrequency)
    req(input$parametersFrequency)
    invalidateLater(1000, session)
    
    
    iterationsFrequencyParameters <- seq(input$iterationFrequency[1],input$iterationFrequency[2])
    print(iterationsFrequencyParameters)
    
    
    conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterationsFrequencyParameters)
    max <- 12
    limit <- 1
    params <- c()
    numberOfParameters <- ceiling(length(input$parametersFrequency)/max)
    for(i in 1: numberOfParameters)
    {
      k <- 1
      for(j in limit:(max*i))
      {
        if(length(input$parametersFrequency) >= j)
        {
          params[k] <- input$parametersFrequency[j]
          k <- k + 1
        }
      }
      
      fixFormat <- iraceResults$parameters
      fixFormat$names <- params
      
      png(filename = paste0("plotFrequency",i,".png"), width = 550, height = 555, res = 80)
      print(png(filename <- paste0("plotFrequency", i, ".png"), width = 550, height = 555, res = 80))
      parameterFrequency(conf, fixFormat)
      dev.off()
      print(dev.off())
      limit <- (max*i) + 1;
    }
    finalPlot <- NULL
    for(i in 1:numberOfParameters)
    {
      if(is.null(finalPlot))
      {
        finalPlot <- image_read(paste0("plotFrequency",i,".png"))
        next
      }
      image <- image_read(paste0("plotFrequency",i,".png"))
      print(image)
      finalPlot <- image_append(c(finalPlot, image), stack = TRUE)
    }
    removeTemporalPlots()
    image_write(finalPlot, path = "../resources/images/frequencyPlot.png", format = "png")
    print(image_write(finalPlot, path = "../resources/images/frequencyPlot.png", format = "png"))
    list(src = "../resources/images/frequencyPlot.png")
  })
  
    output$paralelCoordinatesCandidates <- renderImage({
    req(input$iterationPC)
    req(input$parametersParallelCoordinates)
    invalidateLater(1000, session)
    
    iterationsPC <- seq(input$iterationPC[1],input$iterationPC[2])
    
    last <- length(iraceResults$iterationElites)
    conf <- getConfigurationByIteration(iraceResults = iraceResults,iterations = unique(iterationsPC))
    
    max <- length(iraceResults$parameters$names)
    limit <- 1
    params <- c()
    numberOfParameters <- ceiling(length(input$parametersParallelCoordinates)/max)
    for(i in 1: numberOfParameters)
    {
      k <- 1
      for(j in limit:(max*i))
      {
        if(length(input$parametersParallelCoordinates) >= j)
        {
          params[k] <- input$parametersParallelCoordinates[j]
          k <- k + 1
        }
      }
      
      # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
      fixFormat <- iraceResults$parameters
      fixFormat$names <- params
      
      png(filename = paste0("tempPlotParallel", i, ".png"))
      parallelCoordinatesPlot (conf, fixFormat, hierarchy = FALSE)
      dev.off()
      print(dev.off)
      limit <- (max*i) + 1;
    }
    finalPlot <- NULL
    for(i in 1:numberOfParameters)
    {
      if(is.null(finalPlot))
      {
        finalPlot <- image_read(paste0("tempPlotParallel",i,".png"))
        next
      }
      image <- image_read(paste0("tempPlotParallel", i, ".png"))
      finalPlot <- image_append(c(finalPlot, image), stack = TRUE)
    }
    removeTemporalPlots()
    image_write(finalPlot, path = "../resources/images/parallelPlot.png", format = "png")
    list(src = "../resources/images/parallelPlot.png")
  })
    
    output$boxPlotBestConfiguration <- renderPlot({
      req(input$iterationBoxPlot)
      print(input$iterationBoxPlot)
      invalidateLater(1000, session)
      iterationsBoxPlot <- seq(input$iterationBoxPlot[1],input$iterationBoxPlot[2])
      configurationID <- unique(unlist(iraceResults$allElites[iterationsBoxPlot]))
      results <- iraceResults$experiments[,configurationID,drop = FALSE]
      conf <- gl(ncol(results),
                 nrow(results),
                 labels = colnames(results)
              )
      pairwise.wilcox.test(as.vector(results), conf,paired = TRUE, p.adj ="bonf")
      configurationsBoxplot(results, ylab = "Solution Cost")
    })
    
      output$performance <- renderPlot({
        invalidateLater(1000, session)
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

