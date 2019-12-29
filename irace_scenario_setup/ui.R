if (!require("shiny")) install.packages("shiny", dependencies = TRUE) 
if (!require("shinydashboard")) install.packages("shinydashboard", dependencies = TRUE) 
if (!require("DT")) install.packages("DT", dependencies = TRUE) 
if (!require("shinythemes")) install.packages("shinythemes", dependencies = TRUE) 
if (!require("shinyjs")) install.packages("shinyjs", dependencies = TRUE) 
if (!require("shinyalert")) install.packages("shinyalert", dependencies = TRUE) 
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)
if (!require("irace")) install.packages("irace", dependencies = TRUE) 


library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(shinyBS)
library(data.table)
library(shinyalert)
library(lubridate)
library(irace)

rm(list = ls())
useShinyalert()


skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")devtools::install_github("r-lib/pkgbuild")
skin <- "black"

jscode <- "shinyjs.closewindow = function() { window.close(); }"
assign("flagStop", FALSE, envir=.GlobalEnv,inherits = FALSE)

###Client side
sidebar <- dashboardSidebar( useShinyjs(),
                            extendShinyjs(text = jscode),
                             sidebarMenu(
                                 menuItem("Options", tabName = "options", icon = icon("dashboard")),
                                 menuItem("Parameters", icon = icon("table"), tabName = "parametersTab"),
                                 menuItem("Instances", icon = icon("bar-chart-o"), tabName = "instances"
                                 ),
                                 menuItem("Target Options", icon = icon("file-code-o"),
                                          menuSubItem("Target", tabName = "target")
                                 ),
                                 br(),
                                 actionButton("saveData", "Save configuration", icon = icon("save")),
                                 br(),
                                 disabled(actionButton("start", "Start", icon = icon("play"))),
                                 br(),
                                 actionButton("infoModal","Info", icon = icon("info-circle"))
                             )
)

body <- dashboardBody(
    tabItems(
        # Contenido de secciones
        tabItem(tabName = "options",
                mainPanel(actionButton("saveOptions","Save Options"),downloadButton("exportOptions", "Export", class="butt"), actionButton(inputId='irace_manual', label="User guide", 
                                                                                                                                           icon = icon("external-link-alt"), 
                                                                                                                                           onclick ="window.open('https://cran.r-project.org/web/packages/irace/vignettes/irace-package.pdf', '_blank')")
                          ,tags$hr(),
                          tabsetPanel(
                              tabPanel("General Options",
                                       br(),
                                       shinyjs::useShinyjs(),
                                       disabled(textInput("parameterFile", "Parameter File",'"../shared/parameters.txt"')),
                                       disabled(textInput("trainInstancesDir","Training Instances Dir", '""')),
                                       disabled(textInput("trainInstancesFile","Training Instances File",'"../shared/instances.txt"')),
                                       disabled(textInput("scenarioFile","Scenario File",'"../shared/scenario.txt"')),
                                       disabled(textInput("execDir","Exec Dir",'"../shared/acotsp-arena"')),
                                       disabled(textInput("logFile","Log File",'"../acotsp-arena/irace.Rdata"')),
                                       numericInput("debugLevel","Debug Level",0),
                                       textInput("seed","Seed",""),
                                       textInput("repairConfiguration","Repair Configuration",'""'),
                                       numericInput("postSelection","Post Selection",0, min = 0, max = 100),
                                       checkboxInput("aclib","Aclib",FALSE),
                                       bsTooltip(id = "saveOptions", title = "Save options in working directory as scenario.txt", 
                                                 placement = "right", trigger = "hover")
                              ), 
                              tabPanel("Elitist Race",
                                       br(),
                                       checkboxInput("elitist","Elitist",TRUE),
                                       numericInput("elitistLimit","Elitist Limit",2),
                                       numericInput("elitistNewInstances","Elitist New Instances",1)
                              ), 
                              tabPanel("Internal Irace Options",
                                       br(),
                                       numericInput("maxExperiments","Max Experiments", 5000),
                                       numericInput("nbIterations","Nb Iterations",0),
                                       numericInput("nbExperimentsPerIteration","Nb Experiments per Iteration",0),
                                       numericInput("minNbSurvival","Min Nb Survival",0),
                                       numericInput("nbConfigurations","Nb Configurations",0),
                                       numericInput("mu","Mu",5),
                                       checkboxInput("softRestart","SoftRestart",TRUE)
                                       
                                       
                              )
                          )),
                
                
        ),
        
        ###
        tabItem(tabName = "parametersTab",
                h1("Parameters"),
                actionButton(inputId = "add_param",label = "Add", icon = icon('plus')),
                actionButton(inputId = "edit_param",label = "Edit", icon = icon('edit')),
                actionButton(inputId = "delete_param",label = "Delete", icon = icon('minus')),
                actionButton("update_data","Save", icon = icon("save")),
                br(),
                useShinyalert(),
                uiOutput("parametersTable"),
                downloadButton("data_csv", "Download", class="butt"),
                bsTooltip(id = "update_data", title = "Save parameters in the working directory as rds file", 
                          placement = "right", trigger = "hover")
                
                
        ),
        tabItem(tabName = "instances",
                
                h1("Instances"),
                
                fileInput("file1", "Select a file"),tags$b("Path:"),verbatimTextOutput("instancesPath"),
                verbatimTextOutput("instancesFile")
                
                
        ),
        
        tabItem(tabName = "tParameters",
                h1("Test parameters"),
        ),
        
        tabItem(tabName = "target",
                h1("Target "),
                fileInput('file2',   'Select a file'),tags$b("Path:"),verbatimTextOutput("targetPath"),
                
                textAreaInput(inputId="query", "File content", width='200%', height ='480px',
                              "#Upload a file to edit content"),
                
                actionButton("saveTarget", "Save", icon = icon('save')),
                
                
                h4("Original data:"),
                verbatimTextOutput("targetScript")
                
        )
    )
)




notifications <- dropdownMenu(type = "notifications", badgeStatus = "warning", 
                              taskItem(value = 50, color = "yellow","Waiting")
)



header <- dashboardHeader(
    title = "IRACE",
    tags$li(class = "dropdown", actionLink("setup", "Setup", Style = "color: #ff8000")),
    tags$li(class = "dropdown", actionLink("runtime", "Runtime")),
    tags$li(class = "dropdown", actionLink("reports", "Reports")),
    tags$li(class = "dropdown", actionLink("status", "Status Waiting", icon("circle"), style="color: #ff8000"))
    
)



ui <- dashboardPage(header, sidebar, body, skin = skin)
