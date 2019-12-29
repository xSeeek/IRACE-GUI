library(shiny)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(irace)
library(shinyalert)

useShinyalert()


#skin <- Sys.getenv("DASHBOARD_SKIN")
#skin <- tolower(skin)
#if (skin == "")
#  skin <- "black"

jscode <- "shinyjs.closewindow = function() { window.close(); }"
assign("flagStop", FALSE, envir=.GlobalEnv,inherits = FALSE)

header <- dashboardHeader(
  title = "IRACE",
  tags$li(class = "dropdown",actionLink("status", "Setup")),
  tags$li(class = "dropdown",actionLink("status", "Runtime")),
  tags$li(class = "dropdown",actionLink("status", "Reports")),
  tags$li(class = "dropdown", actionLink("status", "Running", icon("circle"), style="color: #cb3234"))
) 
sidebar <- dashboardSidebar(
        sidebarMenu(
          menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
          menuItem("Performance", icon = icon("th"), tabName = "performance"),
          menuItem("Frequency",tabName = "frequency" ,icon = icon("bar-chart-o")),
          menuItem("Info", icon = icon("info"), href = "http://iridia.ulb.ac.be/irace/"),
          menuItem("Back to Setup", icon = icon("file-code-o"), tabName = "target"),
          actionButton("finish", "Finish IRACE", icon = icon("times-circle"),style="color: #FF0000; background-color: #ffffff; border-color: #ffffff")
        )
)
  body <- dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName = "summary",
        fluidRow(
          box(title="Summary",
              status="primary",
              h5("Irace Version: ", textOutput("iraceVersion",inline=TRUE)),
              h5("Test: ", textOutput("processFinish",inline=TRUE)),
              #h5("Time of Execution: ", textOutput("timeOfExecution",inline=TRUE), " seconds"),
              h5("Num of Iterations: ", textOutput("numIterations",inline=TRUE)),
              h5("Num of Parameters: ", textOutput("numOfParameters",inline=TRUE)),
              h5("Num of Configurations: ",textOutput("numConfigurations",inline=TRUE)),
              h5("Num of Experiments Used so Far: ", textOutput("experimentsUsedSoFar",inline=TRUE), " / ", textOutput("maxExperiments",inline=TRUE)),
              h5("Num of Instances Used so Far: ", textOutput("numInstancesUsedSoFar",inline=TRUE), " / ", textOutput("numOfInstances",inline=TRUE)),
              h5(numericInput("iterationForElites","Select Iteration: ",value = 1,min = 1,max = iraceResults$state$nbIterations,width = "100px"),"Num of Elites Configurations: ",textOutput("numElitesConfigurations",inline=TRUE)),
          )
        ),
          fluidRow(
            box(title = "Elite Configurations",
                status = "primary",
                numericInput("iterationsElites","Select Iteration",value = 1,min = 1,max = iraceResults$state$nbIterations, width = "100px"),
                DT::dataTableOutput("elites"),
                width = 15
            )
          ),
        fluidRow(
          box(title="All Configurations",
              status="primary",
              DT::dataTableOutput("dataTableAllConfigurations"),
              width = 15
          )
        )
    ),
    tabItem(tabName = "performance",
      fluidRow(
        box(title="Performance",
            status="primary",
            h1("BoxPlot"),
            sliderInput("iterationBoxPlot","Select Iteration", min = 1, max = iraceResults$state$nbIterations, value = seq(1,3), dragRange = TRUE),
            plotOutput("boxPlotBestConfiguration"),
            h1("Peformance Plot"),
            plotOutput("performance")
        )
      )
    ),
    tabItem(tabName = "frequency",
      fluidRow(
        tags$div(style = 'overflow-y: auto;',
          box(title = "Frequency",
              status = "primary",
              h1("Sampling Frequency"),
              sliderInput("iterationFrequency","Select Iteration: ",min = 1, max = iraceResults$state$nbIterations,value = seq(1,5),dragRange = TRUE),
              selectInput("parametersFrequency", "Parameters to be displayed: ", iraceResults$parameters$names, multiple = TRUE, width = 2500, selectize = TRUE),
              plotOutput("frecuencyParameters")
          )
        ),
        tags$div(style = 'overflow-y: auto;',
          box(status = "primary",
              h1("Parallel Coordinates"),
              sliderInput("iterationPC","Select Iteration: ",min = 1, max = iraceResults$state$nbIterations, value = seq(1,3), dragRange = TRUE),
              selectInput("parametersParallelCoordinates", "Parameters to be displayed: ", iraceResults$parameters$names, multiple = TRUE, width = 2500, selectize = TRUE),
              plotOutput("paralelCoordinatesCandidates")
          )
        )
      )
    )
  )
)




ui <- dashboardPage(header,sidebar, body)