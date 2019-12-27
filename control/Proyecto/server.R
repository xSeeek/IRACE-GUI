library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(readr)
library(magick)
library(irace)
absolutePath <- getwd()
print(absolutePath)
setwd('../')
load(file = pathRDATA, envir=.GlobalEnv)
updateFile <- function()
{
  load(file = pathRDATA, envir=.GlobalEnv)
  return(irace)
}
removeTemporalPlots <- function(patternData)
{
  junk <- dir(pattern=patternData)
  file.remove(junk)
}
summary <- shinyServer(function(input,output,session){
  
  conf <- length(iraceResults$allConfigurations$.ID.)
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()
  
repeat{   
  withProgress({
    message = "Executing IRACE"
    incProgress(1/15,message = "Rendering Tables")
    Sys.sleep(0.5)
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
      incProgress(2/15, message = "Updating Summary")
      Sys.sleep(0.5)
      #### SUMMARY ####
      output$numOfParameters <- renderText({
        invalidateLater(4000,session)
        updateFile()
        length(iraceResults$parameters$names)
      })
      
      
      output$iraceVersion <- renderText({
        invalidateLater(4000,session)
        iraceResults$irace.version
      })
      
      output$experimentsUsedSoFar <- renderText({
        invalidateLater(4000,session)
        
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
      incProgress(3/15, message = "Rendering Plots")
      Sys.sleep(0.5)
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
          
          png(filename = paste0("tempPlotFrequency",i,".png"), width = 550, height = 555, res = 80)
          print(png(filename <- paste0("tempPlotFrequency", i, ".png"), width = 550, height = 555, res = 80))
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
            finalPlot <- image_read(paste0("tempPlotFrequency",i,".png"))
            next
          }
          image <- image_read(paste0("tempPlotFrequency",i,".png"))
          print(image)
          finalPlot <- image_append(c(finalPlot, image), stack = TRUE)
        }
        removeTemporalPlots('tempPlotFrequency')
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
        
        max <- 12
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
        removeTemporalPlots('tempPlotParallel')
        image_write(finalPlot, path = "../resources/images/parallelPlot.png", format = "png")
        list(src = "../resources/images/parallelPlot.png")
      })
        output$boxPlotBestConfiguration <- renderPlot({
          req(input$iterationBoxPlot)
          invalidateLater(4000, session)
          iterationsBoxPlot <- seq(input$iterationBoxPlot[1],input$iterationBoxPlot[2])
          print(iterationsBoxPlot)
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
    if(iraceResults$state$completed)
    {
      assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
      js$closewindow()
      status <- list(goto = 1)
      stopApp(returnValue = invisible(status))
      break
    }
}

  observe({
    updateSliderInput(session, "iterationPC", min = 1, max = iraceResults$state$nbIterations, value = seq(1,3))
    updateSliderInput(session, "iterationFrequency", min = 1, max = iraceResults$state$nbIterations, value = seq(1,3))
    updateSliderInput(session, "iterationPerformance", min = 1, max = iraceResults$state$nbIterations, value = seq(1,3))
    updateSliderInput(session, "iterationBoxPlot", min = 1, max = iraceResults$state$nbIterations, value = seq(1,3))
    updateNumericInput(session, "iterationForElites", value = 1,min = 1, max = iraceResults$state$nbIterations)
    updateNumericInput(session, "iterationsElites", value = 1,min = 1, max = iraceResults$state$nbIterations)
    updateSelectInput(session,"parametersParallelCoordinates",choices = iraceResults$parameters$names)
    updateSelectInput(session,"parametersFrequency",choices = iraceResults$parameters$names)
  })

  session$onSessionEnded(function() {
        if(flagStop == FALSE)
        {
            print('SESSION ENDED BY SETUP APP')
            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            stopApp()
        }
    })
})
