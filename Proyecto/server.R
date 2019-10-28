library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)

server <- function(input, output) {
  resourcesPath <- paste(absolutePath, "../resources", sep = "")
  absolutePath <- getwd()
  
  addResourcePath(prefix = 'resources', directoryPath = '../resources')
  load('../resources/irace.Rdata', envir=.GlobalEnv)
  
  print(resourcesPath)
}

summary <- function(input,output){
  
  iterations <- iraceResults$state$nbIterations
  count <- 0
  #last <- length(iraceResults$iterationElites)
 # id<-iraceResults$iterationElites[last]
  #elites <- getConfigurationById(iraceResults,ids = id)
  
 # results <- iraceResults$testing$experiments
 #conf <- gl(ncol(results),
    #         nrow(results),
   #          labels = colnames(results))
 # pairwise.wilcox.test(as.vector(results), conf, paired=TRUE, p.adj ="bonf")
  
  
  
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
    for(i in 1:length(iraceResults$allElites[iterations]))
    {
      for(n in 1:length(iraceResults$allElites[[iterations]]))
      {
        count = count + 1
      }
      return(count)
    }
  )
  #output$selectedIteration <- renderUI({
   # req(input$iteration)
    #bestConfigurations <- iraceResults$allElites[as.integer(input$iteration)]
    #bestConfigurationID <- bestConfigurations[[1]][1]
    #parametersBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
    #dataTable = dataTableEliteConfiguration(bestConfigurations[[1]])
  #})
  #output$elites <- DT::renderDataTable(
    #elites
  #)    
  output$dataTableElites <- DT::renderDataTable(
    iraceResults$allConfigurations,
    options = list(
      scrollX = TRUE,
      pageLength = 5
    )
  )
  #output$boxPlot <- renderPlot(
    #boxplot(results,
         #   main = "Boxplot of the testing results of the best configurations",
         #   xlab = "Configuration ID",
          #  ylab = "Solution Cost",
          #  horizontal = TRUE,
         #   notch = TRUE
    #)
  #)
}