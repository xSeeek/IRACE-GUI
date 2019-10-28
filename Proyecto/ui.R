library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(ggplot2)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"

header <- dashboardHeader(
  title="IRACE"
) 
sidebar <- dashboardSidebar(
        sidebarMenu(
          menuItem("Modules", tabName ="modules",
                   menuSubItem("Setup", tabName="setup"),
                   menuSubItem("Runtime", tabName="runtime"),
                   menuSubItem("Reports", tabName="reports")
          ),
          menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
          menuItem("Performance", icon = icon("th"), tabName = "performance"),
          menuItem("Frequency", icon = icon("bar-chart-o"),
            menuSubItem("Chart sub-item 1", tabName = "subitem1"),
            menuSubItem("Chart sub-item 2", tabName = "subitem2")
          ),
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
            textOutput("numIterations"),
            "Num of Configurations",
            textOutput("numConfigurations"),
            "Num of Instances",
            textOutput("numInstances"),
            "Num of Elites Configurations",
            textOutput("numElitesConfigurations")
        )
      ),
      fluidRow(
        box(title="All Configurations",
            status="primary",
            DT::dataTableOutput("dataTableElites"),
            width = 15
        )
      )
    )
  ),
  tabItems(
    tabItem(tabName = "performance",
      fluidRow(
        box(title="Performance",
            status="primary",
            plotOutput("boxPlot")
        )
      )
    )
  )
)


ui <- dashboardPage(header,sidebar, body, skin = skin)