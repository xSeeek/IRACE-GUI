library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)
library(irace)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"

header <- dashboardHeader(
  title="IRACE",
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
          menuItem("Back to Setup", icon = icon("file-code-o"), tabName = "target")
        )
)
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
        fluidRow(
          box(title="Summary",
              status="primary",
              "Num of Iterations: ",
              verbatimTextOutput("numIterations"),
              "Num of Configurations",
              textOutput("numConfigurations"),
              "Num of Instances",
              textOutput("numInstances"),
              "Num of Elites Configurations",
              textOutput("numElitesConfigurations")
          )
        ),
          fluidRow(
            box(title = "Elite Configurations",
                status = "primary",
                numericInput("iterationsElites","Select Iteration",value = 1,min = 1,max = iraceResults$state$nbIterations),
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
            sliderInput("iterationBoxPlot","Select Iteration", min = 1, max = iraceResults$state$nbIterations, value = 1, dragRange = TRUE),
            plotOutput("boxPlotBestConfiguration"),
            h1("Peformance Plot"),
            plotOutput("performance")
        )
      )
    ),
    tabItem(tabName = "frequency",
      fluidRow(
        box(title = "Frequency",
            status = "primary",
            h1("Sampling Frequency"),
            sliderInput("iterationFrequency","Select Iteration",min = 1, max = iraceResults$state$nbIterations,value = c(1,2),dragRange = TRUE),
            plotOutput("frecuencyParameters"),
            h1("Parallel Coordinates"),
            sliderInput("iterationPC","Select Iteration",min = 1, max = iraceResults$state$nbIterations, value = c(1,2), dragRange = TRUE),
            plotOutput("paralelCoordinatesCandidates")
        )
      )
    )
  )
)




ui <- dashboardPage(header,sidebar, body, skin = skin)