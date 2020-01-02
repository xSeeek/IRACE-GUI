library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(dashboardthemes)
library(DT)
library(ggplot2)
library(irace)
library(shinyalert)
library(shinyBS)

useShinyalert()
useShinyjs()

#skin <- Sys.getenv("DASHBOARD_SKIN")
#skin <- tolower(skin)
#if (skin == "")
#  skin <- "black"

jscode <- "shinyjs.closewindow = function() { window.close(); }"
assign("flagStop", FALSE, envir=.GlobalEnv,inherits = FALSE)

header <- dashboardHeader(
  title = "IRACE",
  dropdownMenuOutput("status"),
  tags$li(class = "dropdown",actionLink("setup", "Setup")),
  tags$li(class = "dropdown",actionLink("runtime", "Runtime",style="color: #cb3234")),
  tags$li(class = "dropdown",actionLink("reports", "Reports"))
) 
sidebar <- dashboardSidebar(
        sidebarMenu(
          menuItem("Summary", tabName = "summary", icon = icon("book-open")),
          menuItem("Performance", icon = icon("bar-chart-o"), tabName = "performance"),
          menuItem("Frequency",tabName = "frequency" ,icon = icon("bar-chart-o")),
          menuItem("Info", icon = icon("info"), href = "http://iridia.ulb.ac.be/irace/"),
          menuItem("Back to Setup", icon = icon("undo"), tabName = "target"),
          actionButton("finish", "Finish IRACE", icon = icon("times-circle"),style="color: #FF0000; background-color: #ffffff; border-color: #ffffff"),
          disabled(actionButton("change", "Go to Reports",icon = icon("file-alt"),style="color: #328900; background-color: #ffffff; border-color: #ffffff"))
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
          actionButton("summary","",icon = icon("info-circle"),style="color: #f3d603; background-color: #ffffff; border-color: #ffffff"),
          bsTooltip(id = "summary",title= "This section shows a summary of the execution of IRACE and shows te values of the variables in the scenario. Also shows a table with Elite Configurations and a table with All Configurations",placement ="right",trigger = "hover"),
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
              h5(numericInput("iterationForElites","Select Iteration: ",value = 1,min = 1,max = iraceResults$state$nbIterations,width = "100px"),"Num of Elites Configurations: ",textOutput("numElitesConfigurations",inline=TRUE))
          ),
          
        ),
          fluidRow(
            actionButton("elites","",icon = icon("info-circle"),style="color: #f3d603; background-color: #ffffff; border-color: #ffffff"),
            bsTooltip(id = "elites",title= "This section shows a table with All the Elite Configurations with the values ​​of their parameters",placement ="right",trigger = "hover"),
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
          ),
          actionButton("allConf","",icon = icon("info-circle"),style="color: #f3d603; background-color: #ffffff; border-color: #ffffff"),
          bsTooltip(id = "allConf",title= "This section shows a table with All Configurations with the values of their parameters",placement ="right",trigger = "hover")
        )
    ),
    tabItem(tabName = "performance",
      fluidRow(
        actionButton("performance","",icon = icon("info-circle"),style="color: #f3d603; background-color: #ffffff; border-color: #ffffff"),
        bsTooltip(id = "performance",title= "This section shows the quality of the configurations per iterations. The boxplot shows the median of each elite configuration for each range of iterations",placement ="right",trigger = "hover"),
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
          ),
          actionButton("frequency","",icon = icon("info-circle"),style="color: #f3d603; background-color: #ffffff; border-color: #ffffff"),
          bsTooltip(id = "frequency",title= "This section shows a parallel coordinates plot and a frequency plot that shows how the values of parameters are varying as the iterations are progressing",placement ="right",trigger = "hover")
        ),
        tags$div(style = 'overflow-y: auto;',
          box(status = "primary",
              h1("Parallel Coordinates"),
              sliderInput("iterationPC","Select Iteration: ",min = 1, max = iraceResults$state$nbIterations, value = seq(1,3), dragRange = TRUE),
              selectInput("parametersParallelCoordinates", "Parameters to be displayed: ", iraceResults$parameters$names, selected = iraceResults$parameters$names, multiple = TRUE, width = 2500, selectize = TRUE),
              plotOutput("paralelCoordinatesCandidates")
          )
        )
      )
    )
  )
)




ui <- dashboardPage(header,sidebar, body)