if (!require("shiny")) install.packages("shiny", dependencies = TRUE) 
if (!require("shinydashboard")) install.packages("shinyjs", dependencies = TRUE) 
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



###Client side
sidebar <- dashboardSidebar( useShinyjs(),
                             sidebarMenu(
                                 menuItem("Options", tabName = "options", icon = icon("dashboard")),
                                 menuItem("Parameters", icon = icon("table"), tabName = "parametersTab"),
                                 menuItem("Instances", icon = icon("bar-chart-o"), tabName = "instances"
                                 ),
                                 menuItem("Target Options", icon = icon("file-code-o"),
                                          menuSubItem("Target", tabName = "target"),
                                          menuSubItem("Chart sub-item 2", tabName = "tParameters")
                                 ),
                                 br(),
                                 actionButton("saveData", "Save configuration", icon = icon("save")),
                                 br(),
                                 disabled(actionButton("start", "Start", icon = icon("play")))
                             )
)

body <- dashboardBody(
    tabItems(
        # Contenido de secciones
        tabItem(tabName = "options",
                mainPanel(actionButton("saveOptions","Save Options"),disabled(actionButton("editOptions","Edit scenario.txt")),tags$hr(),
                          tabsetPanel(
                              tabPanel("General Options",
                                       br(),
                                       shinyjs::useShinyjs(),
                                       disabled(textInput("scenarioFile","Scenario File",'"../Irace_scenario/scenario.txt"')),
                                       disabled(textInput("execDir","Exec Dir",'"./Irace_scenario"')),
                                       disabled(textInput("logFile","Log File",'"./Irace_scenario/irace.Rdata"')),
                                       numericInput("debugLevel","Debug Level",0),
                                       textInput("seed","Seed",""),
                                       textInput("repairConfiguration","Repair Configuration",""),
                                       numericInput("postSelection","Post Selection",0),
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
                
                # h1("Instances "),
                # fileInput('file2',   'Select a file'),tags$b("Path:"),verbatimTextOutput("instancesPath"),
                # 
                # textAreaInput(inputId="query", "File content", width='200%', height ='480px',
                #                 "#Upload a file to edit content"),
                # 
                # actionButton("saveInstances", "Save", icon = icon('save')),
                # 
                # 
                # h4("Original data:"),
                # verbatimTextOutput("instancesFile")
                
        ),
        
        tabItem(tabName = "tParameters",
                h1("Test parameters"),
        ),
        
        tabItem(tabName = "target",
                # h1("Target"),
                # 
                # fileInput("file1", "Select a file"),tags$b("Path:"),verbatimTextOutput("targetPath"),
                # verbatimTextOutput("targetScript")
                
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
    tags$li(class = "dropdown", actionLink("setup", "Setup")),
    tags$li(class = "dropdown", actionLink("runtime", "Runtime")),
    tags$li(class = "dropdown", actionLink("reports", "Reports")),
    tags$li(class = "dropdown", actionLink("status", "Status Waiting", icon("circle"), style="color: #ff8000"))
    
)



ui <- dashboardPage(header, sidebar, body, skin = skin)


###Server Side
server <- function(input, output, session) {
    
    valuesTable<-reactiveValues()
    valuesTable$Data<-readRDS("note.rds")
    
    scenarioData <- data.frame()
    scenarioResults <- reactive(data.frame(input$debugLevel
    ))
    
    observeEvent(input$saveOptions,{
        #Append the row in the dataframe
        scenarioData <<- rbind(scenarioData,scenarioResults()) 
        
        write.table(scenarioData,"../scenario.txt",row.names = FALSE,  sep = '', quote = F)
        
        
    })
    
    
    
    
    
    output$parametersTable<-renderUI({
        fluidPage(
            hr(),
            dataTableOutput("mainTable"),
            tags$script("$(document).on('click', '#mainTable button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random()) });")
            
        ) 
    })
    
    
    output$mainTable<-renderDataTable({
        DT=valuesTable$Data
        datatable(DT,selection = 'single',
                  escape=F, rownames = FALSE) 
    })
    
    
    observeEvent(input$add_param, {
        showModal(modalDialog(title = "Add a new parameter",
                              textInput("Name_add", "Name"),
                              textInput("Flag_add", "Flag"),
                              selectInput("Type_add", "Type:",
                                          c("Real" = "r",
                                            "Integer" = "i",
                                            "Cardinal" = "c",
                                            "ordinal" = "o",
                                            "RealLog" = "r,log",
                                            "IntegerLog" = "i,log"
                                          )),
                              textInput("Domain_add", "Domain"),
                              textInput("Conditions_add", "Conditions"), 
                              actionButton("add_item", "Add item"),
                              
                              easyClose = TRUE, footer = NULL ))
        
    })
    
    observeEvent(input$add_item, {
        validate(
            need(input$Name_add != "", "Please insert a name"),
            need(input$Flag_add != "", "Please insert a flag"),
            need(input$Type_add != "", "Please insert a flag"),
            need(input$Domain_add != "", "Please insert domain"),
            need(input$Conditions_add != "", "Please insert conditions")
            
        )
        
        new_row=data.frame( Name=input$Name_add,
                            Flag=input$Flag_add,
                            Type=input$Type_add,
                            Domain=input$Domain_add,
                            Conditions=input$Conditions_add )
        
        valuesTable$Data<-rbind(valuesTable$Data, new_row )
        removeModal()
    })
    
    
    
    
    ### save to RDS part 
    observeEvent(input$update_data,{
        saveRDS(valuesTable$Data, "note.rds")
        shinyalert(title = "Data saved", type = "success")
    })
    
    ### delete selected rows part
    ### this is warning messge for deleting
    observeEvent(input$delete_param,{
        showModal(
            if(length(input$mainTable_rows_selected)>=1 ){
                modalDialog(
                    title = "Warning",
                    paste("Are you sure delete",length(input$mainTable_rows_selected),"rows?" ),
                    footer = tagList(
                        modalButton("Cancel"),
                        actionButton("ok", "Yes")
                    ), easyClose = TRUE)
            }else{
                modalDialog(
                    title = "Warning",
                    paste("Please select the parameter that you want to delete!" ),easyClose = TRUE
                )
            }
            
        )
    })
    
    
    observeEvent(input$ok, {
        valuesTable$Data=valuesTable$Data[-input$mainTable_rows_selected]
        removeModal()
    })
    
    ### Edit modal
    observeEvent(input$edit_param,{
        showModal(
            if(length(input$mainTable_rows_selected)>=1 ){
                modalDialog(
                    fluidPage(
                        h3(strong("Modification"),align="center"),
                        hr(),
                        dataTableOutput('row_modif'),
                        br(),
                        actionButton("save_changes","Save changes"),
                        tags$script(HTML("$(document).on('click', '#save_changes', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++)
                             {
                             list_value.push($( '.new_input' )[i].value)
                             }
                             Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
            }else{
                modalDialog(
                    title = "Warning",
                    paste("Please select the parameter to delete" ),easyClose = TRUE
                )
            }
            
        )
    })
    
    
    
    
    ### Edit
    output$row_modif<-renderDataTable({
        selected_row=input$mainTable_rows_selected
        old_row=valuesTable$Data[selected_row]
        row_change=list()
        for (i in colnames(old_row))
        {
            if (is.numeric(valuesTable$Data[[i]]))
            {
                row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
            } 
            else{ 
                row_change[[i]]<-paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
            }
        }
        row_change=as.data.table(row_change)
        setnames(row_change,colnames(old_row))
        DT=row_change
        DT 
    },escape=F,options=list(dom='t',ordering=F,scrollX = TRUE),selection="none" )
    
    
    
    ### This replace the row
    observeEvent(input$newValue,
                 {
                     newValue=lapply(input$newValue, function(col) {
                         if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                             as.numeric(as.character(col))
                         } else {
                             col
                         }
                     })
                     DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                     colnames(DF)=colnames(valuesTable$Data)
                     valuesTable$Data[input$mainTable_rows_selected]<-DF
                 }
    )
    
    output$data_csv<- downloadHandler(
        filename = function() {
            paste("irace parameters", ".csv", sep="")
        },
        content = function(file) {
            write.csv(data.frame(valuesTable$Data), file, row.names = F)
        }
    )
    
    
    ### Files Creation and execution of IRACE
    savedata <- observe({
        input$saveData
        input$saveTarget
        
        # input$saveOptions
        # input$editOptions
        
        
        iraceFile = file.path("../Irace_scenario")
        # ScenarioFileOpt = file.path("../Irace_scenario/scenario.txt")
        
        ### if the directory does not exist, it is created
        
        if(!file.exists(iraceFile)){
            dir.create(file.path("../Irace_scenario"))
            
        }
        
        
        ### the directory is set in the created folder
        setwd("../Irace_scenario") 
        
        ### If the button is pressed, then the file is created
        if(input$saveData>0){
            x <- data.table(valuesTable$Data)
            
            write.table(x,"../Irace_scenario/parameters.txt",row.names = FALSE, col.names = FALSE,  sep = '\t', quote = F)
            if(is.null(input$file1)) {return()}
            write.table(paste(readLines(input$file1$datapath)),"../Irace_scenario/instances.txt",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
            
            
            
            
            
            
        }
        if(input$saveTarget>0){
            if(is.null(input$file2)) {return()}
            write.table(paste(input$query),"../Irace_scenario/target-runner.r",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
            
        }
        
        if(file.exists(file.path("../Irace_scenario/parameters.txt")) && file.exists(file.path("../Irace_scenario/scenario.txt")) && file.exists(file.path("../Irace_scenario/instances.txt")) && file.exists(file.path("../Irace_scenario/target-runner.r"))){
            enable("start")
        }
        
        
    })
    observeEvent(input$start,{
        if(input$start>0){
            parameters = readParameters(file="parameters.txt")
            scenario = readScenario(filename="scenario.txt")
            irace(scenario=scenario, parameters=parameters)
            
        }
    })
    
    ### This section save all the user inputs on the IRACE options
    observeEvent(input$saveOptions,{
        if(input$saveOptions>0){
            write.table(paste("###General Options\n\n",
                              "scenarioFile=",input$scenarioFile,"\n",
                              "execDir= ",input$execDir,"\n",
                              "logFile=",input$logFile,"\n",
                              "debugLevel=:",input$debugLevel,"\n",
                              "seed=",as.integer(input$seed),"\n",
                              "repairConfiguration=",input$repairConfiguration,"\n",
                              "postSelection=",input$postSelection,"\n",
                              "aclib=",as.integer(input$aclib),"\n\n",
                              "###Elitist Race\n\n",
                              "elitist=",as.integer(input$elitist),"\n",
                              "elitistLimit=",input$elitistLimit,"\n",
                              "elitistNewInstances=",input$elitistNewInstances,"\n",
                              "nbIterations=",input$nbIterations,"\n\n",
                              "###Internal Irace Options\n\n",
                              "nbExperimentsPerIteration=",input$nbExperimentsPerIteration,"\n",
                              "minNbSurvival=",input$minNbSurvival,"\n",
                              "nbConfigurations=",input$nbConfigurations,"\n",
                              "mu=",input$mu,"\n",
                              "softRestart=",as.integer(input$softRestart),"\n"
                              
                              
                              
                              
            ),"../Irace_scenario/scenario.txt",row.names = FALSE,col.names = FALSE, sep = "",quote = F)
            if(file.exists(file.path("../Irace_scenario/scenario.txt"))){
                shinyalert("File Saved!")
                enable("editOptions")
            }
            
        }
    })
    
    observeEvent(input$editOptions,{
        ScenarioFileOpt = file.path("../Irace_scenario/scenario.txt")
        if(input$editOptions>0 && file.exists(ScenarioFileOpt) ){
            showModal(
                modalDialog(title = "Scenario.txt", size = c("m", "s", "l"),
                            textAreaInput(inputId = "scenarioText","",value = paste(readLines("../Irace_scenario/scenario.txt"), collapse = "\n"), width = '190%',
                                          height = '400px', resize = NULL),
                            actionButton("saveEditionOp","Save"),
                            easyClose = TRUE
                )
            )
            
            
            
        }
        
    })
    observeEvent(input$saveEditionOp,{
        if(input$saveEditionOp>0){
            write.table(paste(input$scenarioText),"../Irace_scenario/scenario.txt",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
        }
        shinyalert("File Edited!")
        
        
    })
    
    
    
    
    #Files upload
    
    fileTextScript <- eventReactive(input$file1,{
        file1Path <- input$file1$datapath
        fileTextScript <- paste(readLines(file1Path), collapse = "\n")
    })
    
    fileText <- eventReactive(input$file2, {
        filePath <- input$file2$datapath
        fileText <- paste(readLines(filePath), collapse = "\n")
        
        
        updateTextAreaInput(session, "query", value = fileText)
        return(fileText)
    })
    
    output$instancesFile <- renderText({ fileTextScript() })  
    output$instancesPath <- renderText({input$file1$datapath}, quoted = FALSE)
    
    output$targetScript <- renderText({ fileText() }) 
    output$targetPath <- renderText({input$file2$datapath}, quoted = FALSE)
    
}

shinyApp(ui, server)

