library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(DT)
library(ggplot2)
library(readr)
library(magick)
library(irace)
library(future)
plan(multiprocess)
setwd('../')
print(getwd())
load(pathRDATA, envir=.GlobalEnv)
updateFile <- function()
{
  if(iraceResults$state$completed == TRUE)
  {
    shinyjs::enable("change")
  }
  load(pathRDATA, envir=.GlobalEnv)
  return(irace)
}

removeTemporalPlots <- function(patternData)
{
  junk <- dir(pattern=patternData)
  file.remove(junk)
}

extractPID <- function()
{
  process <- system("ps -ef | grep runIrace.R | grep -v grep | awk '{print $2}'", intern = TRUE)
  substr(process,1,5)
  print(substr(process,1,5))
}
summary <- shinyServer(function(input,output,session){
  
  conf <- length(iraceResults$allConfigurations$.ID.)
  iterations <- iraceResults$state$nbIterations
  count <- 0
  bestConfiguration <- data.frame()
  time <- 0
  statusIrace <- iraceResults$state$completed
  output$status <- renderMenu({
    if(iraceResults$state$completed == FALSE)
    {
      invalidateLater(1000,session)
    }
    if(iraceResults$state$completed == TRUE)
    {
      dropdownMenu(type = "notifications",
        notificationItem(
          text = "Finished",
          icon("circle"),
          status = "success"
        )
      )
    }else{
        dropdownMenu(type = "notifications",
          notificationItem(
            text = "Running",
            icon("circle"),
            status = "warning"
          )
        )
    }
  })
    #### TABLAS ####
      observe({
        if(iraceResults$state$completed == FALSE)
        {

          invalidateLater(4000,session)
        }
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
              isolate(bestConfiguration)
        })
      })
      observe({
        if(iraceResults$state$completed == FALSE)
        {
          invalidateLater(4000,session)
        }
        output$dataTableAllConfigurations <- DT::renderDataTable({
          DT::datatable(iraceResults$allConfigurations,
                        options = list(
                          scrollX = TRUE,
                          scrollY = TRUE,
                          pageLength = 5
                        ))
          isolate(iraceResults$allConfigurations)
        })
      })
      #### SUMMARY ####
        output$numOfParameters <- renderText({
          withProgress(message = "Updating data",value = 0,{
            incProgress(1/10,detail = paste("Getting Data"))
            Sys.sleep(0.3)
            if(iraceResults$state$completed == FALSE)
            {
              invalidateLater(4000,session)
            }
            updateFile()
            incProgress(10/10, detail = paste("Updating Number of Parameters"))
            Sys.sleep(0.3)
            length(iraceResults$parameters$names)
            isolate(length(iraceResults$parameters$names))
        })
      })
        
        
        output$iraceVersion <- renderText({
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000,session)
          }
          iraceResults$irace.version
          isolate(iraceResults$irace.version)
        })
        output$experimentsUsedSoFar <- renderText({
          withProgress(message = "Updating Data", value = 0, {
            incProgress(1/10, detail = "Getting Data")
            Sys.sleep(0.3)
            if(iraceResults$state$completed == FALSE)
            {
              invalidateLater(4000,session)
            }
            incProgress(10/10, detail = paste("Updating Data"))
            Sys.sleep(0.3)
            iraceResults$state$experimentsUsedSoFar
            isolate(iraceResults$state$experimentsUsedSoFar)
          })
        })
      
        output$maxExperiments <- renderText({
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          iraceResults$scenario$maxExperiments
          isolate(iraceResults$scenario$maxExperiments)
        })
        
        output$numIterations <- renderText({
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          iraceResults$state$nbIterations
          isolate(iraceResults$state$nbIterations)
        })
        output$numConfigurations <- renderText({
          withProgress(message = "Updating Data", value = 0, {
            incProgress(1/10,detail = paste("Getting Data"))
            Sys.sleep(0.3)
            if(iraceResults$state$completed == FALSE)
            {
              invalidateLater(4000, session)
            }
            incProgress(10/10,detail = paste("Updating Number of Configurations"))
            Sys.sleep(0.3)
            conf <- length(iraceResults$allConfigurations$.ID.)
            isolate(conf)
          })
        })

        output$numInstancesUsedSoFar <- renderText({
          withProgress(message = "Updating Data", value = 0, {
            incProgress(1/10,detail = paste("Getting Data"))
            Sys.sleep(0.3)
            if(iraceResults$state$completed == FALSE)
            {
              invalidateLater(4000, session)
            }
            incProgress(10/10,detail = paste("Updating Number of Instances used so far"))
            Sys.sleep(0.3)
            nrow(iraceResults$experiments)
            isolate(nrow(iraceResults$experiments))
          })
        })
        output$numOfInstances <- renderText({
          withProgress(message = "Updating Data", value = 0, {
            incProgress(1/10,detail = paste("Getting Data"))
            Sys.sleep(0.3)
            if(iraceResults$state$completed == FALSE)
            {
              invalidateLater(4000,session)
            }
            incProgress(10/10,detail = paste("Updating Number of Instances"))
            Sys.sleep(0.3)
            length(iraceResults$scenario$instances)
            isolate(length(iraceResults$scenario$instances))
          })
        })
        observe({
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          output$numElitesConfigurations <- renderText({
            req(input$iterationForElites)
            for(i in iraceResults$allElites[as.integer(input$iterationForElites)])
            {
              c(i)
              return(length(i))
            }
            isolate(length(i))
          })
        }) 
     #### PLOTS ####
      output$plotPerformance <- renderPlot({
        req(input$iterationPerformance)
        withProgress(message = "Updating Data", value = 0, {
          incProgress(1/10,detail = paste("Getting Data"))
          Sys.sleep(0.1)
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
          elites <- as.character(iraceResults$iterationElites)
          values <- colMeans(iraceResults$testing$experiments[,elites])
          incProgress(10/10,detail = paste("Plotting Plot Performance"))
          Sys.sleep(0.1)
          plot(fes,values,type="s",xlab="Number of runs of the target algorithm",ylab= "Mean value over testing set")
          points(fes,values)
          text(fes,values,elites,pos=1)
        })
      })
      

      output$frecuencyParameters <- renderImage({
        req(input$iterationFrequency)
        req(input$parametersFrequency)
        withProgress(message = "Updating Data", value = 0, {
          incProgress(1/10,detail = paste("Getting Data"))
          Sys.sleep(0.1)
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          
          
          
          conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = input$iterationFrequency[1]:input$iterationFrequency[2])
          print(input$parametersFrequency)
          print(conf)
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
            incProgress(5/10,detail = paste("Plotting Frequency"))
            Sys.sleep(0.1)
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
          print(getwd())
          incProgress(10/10,detail = paste("Showing Plot of Frequency"))
          Sys.sleep(0.1)
          removeTemporalPlots('tempPlotFrequency')
          image_write(finalPlot,path = paste0(getwd(),"/resources/images/frequencyPlot.png"), format = "png")
          print(image_write(finalPlot,path = paste0(getwd(),"/resources/images/frequencyPlot.png"), format = "png"))
          list(src = paste0(getwd(),"/resources/images/frequencyPlot.png"))
        })
      })
        output$paralelCoordinatesCandidates <- renderImage({
        req(input$iterationPC)
        req(input$parametersParallelCoordinates)
        withProgress(message = "Updating Data", value = 0, {
          incProgress(1/10,detail = paste("Getting Data"))
          Sys.sleep(0.1)
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          
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
            incProgress(5/10,detail = paste("Plotting Parallel Coordinates"))
            Sys.sleep(0.1)
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
          incProgress(10/10,detail = paste("Showing Parallel Coordinates Plot"))
          Sys.sleep(0.1)
          image_write(finalPlot,path = paste0(getwd(),"/resources/images/parallelPlot.png"), format = "png")
          list(src = paste0(getwd(),"/resources/images/parallelPlot.png"))
        })
      })
        output$boxPlotBestConfiguration <- renderPlot({
          if(iraceResults$state$completed == FALSE)
          {
            invalidateLater(4000, session)
          }
          req(input$iterationBoxPlot)
          withProgress(message = "Updating Data", value = 0, {
            incProgress(1/10,detail = paste("Getting Data"))
            Sys.sleep(0.1)
            iterationsBoxPlot <- seq(input$iterationBoxPlot[1],input$iterationBoxPlot[2])
            print(iterationsBoxPlot)
            configurationID <- unique(unlist(iraceResults$allElites[iterationsBoxPlot]))
            results <- iraceResults$experiments[,configurationID,drop = FALSE]
            conf <- gl(ncol(results),
                      nrow(results),
                      labels = colnames(results)
                    )
            pairwise.wilcox.test(as.vector(results), conf,paired = TRUE, p.adj ="bonf")
            incProgress(10/10,detail = paste("Plotting BoxPlot"))
            Sys.sleep(0.1)
            configurationsBoxplot(results, ylab = "Solution Cost")
          })
        })
        
          output$performance <- renderPlot({
            if(iraceResults$state$completed == FALSE)
            {
                invalidateLater(4000, session)
            }
            withProgress(message = "Updating Data", value = 0, {
              incProgress(1/10,detail = paste("Getting Data"))
              Sys.sleep(0.1)
              iters <- unique(iraceResults$experimentLog[,"iteration"])
              fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
              elites <- as.character(iraceResults$iterationElites)
              values <- colMeans(iraceResults$experiments[,elites])
              incProgress(10/10,detail = paste("Plotting Performance Plot"))
              Sys.sleep(0.1)
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
    # FINISH IRACE #

    output$processFinish <- renderText({
      system("ps -ef | grep runIrace.R | grep -v grep | awk '{print $2}'", intern = TRUE)
    })

    observeEvent(input$finish,{
      showModal(
        modalDialog(title = "Warning",
                    paste("Are you sure to finish IRACE?"),
                    footer = tagList(
                      modalButton("Cancel"),
                      actionButton("acept_finish","Yes")
                    ),easyClose = TRUE)
      )
    })

    observeEvent(input$acept_finish,{
      assign("flagControl", TRUE, envir=.GlobalEnv,inherits = FALSE)
      system(paste("kill",extractPID()))
      js$closewindow()
      status <- list(goto = 0)
      stopApp(returnValue = invisible(status))
    })

    observeEvent(input$change,{
      assign("flagControl", TRUE, envir=.GlobalEnv,inherits = FALSE)
      js$closewindow()
      status <- list(goto = 1)
      stopApp(returnValue = invisible(status))
    })



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
        print('SESSION ENDED BY CONTROL APP')
        if(flagControl == FALSE)
        {
            session$sendCustomMessage(type = "closeWindow", message = "message")
            stopApp(returnValue = invisible(status))
        }
        assign("flagControl", FALSE, envir=.GlobalEnv,inherits = FALSE)
    })
})
