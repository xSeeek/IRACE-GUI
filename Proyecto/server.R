library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(irace, lib.loc = "/usr/local/lib/R/site-library")
library(readr)
library(magick)
absolutePath <- getwd()
load(file = '../resources/test-dummy/acotsp-arena/irace.Rdata', envir=.GlobalEnv)
updateFile <- function()
{
  load(file = '../resources/test-dummy/acotsp-arena/irace.Rdata', envir=.GlobalEnv)
  return(irace)
}


removeTemporalPlots <- function()
{
  junk <- dir(pattern="tempPlot")
  file.remove(junk)
}

summary <- shinyServer(function(input,output,session){
  
  
  
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()

  
  
  
  
  #### TABLAS ####
  observe({
    invalidateLater(4000,session)
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
  })
  observe({
    invalidateLater(4000,session)
    output$dataTableAllConfigurations <- DT::renderDataTable({
      DT::datatable(iraceResults$allConfigurations,
                    options = list(
                      scrollX = TRUE,
                      scrollY = TRUE,
                      pageLength = 5
                    ))
    })
  })
  
  #### SUMMARY ####
  
  output$numOfParameters <- renderText({
    updateFile()
    invalidateLater(4000,session)
    length(iraceResults$parameters$names)
  })
  
  
  output$iraceVersion <- renderText({
    invalidateLater(4000,session)
    iraceResults$irace.version
  })
  
  
  output$experimentsUsedSoFar <- renderText({
    invalidateLater(4000,session)
    withProgress(message = 'Executing IRACE', value = 0, {
      # Number of times we'll go through the loop
     n <- iraceResults$state$nbIterations
      
      for (i in 1:n) 
        {
        #Increment the progress bar, and update the detail text.
       incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    iraceResults$state$experimentsUsedSoFar
  })
  output$maxExperiments <- renderText({
    invalidateLater(4000, session)
    iraceResults$scenario$maxExperiments
  })
  
  output$numIterations <- renderText({
    invalidateLater(4000, session)
    iraceResults$state$nbIterations
  })
  output$numConfigurations <- renderText({
    invalidateLater(4000, session)
    conf <- length(iraceResults$allConfigurations$.ID.)
  })
  output$numInstancesUsedSoFar <- renderText({
    invalidateLater(4000, session)
    nrow(iraceResults$experiments)
  })
  #output$numOfInstances <- renderText({
    #invalidateLater(4000,session)
    #length(iraceResults$experiments)
  #})
  observe({
    invalidateLater(4000, session)
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
    invalidateLater(4000, session)
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
    invalidateLater(4000, session)
    
    
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
    invalidateLater(4000, session)
    
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
      invalidateLater(4000, session)
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
        invalidateLater(4000, session)
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
})

