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

###Server Side
server <- function(input, output, session) {
    
    valuesTable<-reactiveValues()
    
    if(file.exists(file.path("../shared/note.rds"))){
        valuesTable$Data<-readRDS("../shared/note.rds")
    }else{
        valuesTable$Data<-readRDS("note.rds")
    }
    
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
            need(input$Domain_add != "", "Please insert domain")
            
        )
        
        new_row=data.frame( Name=input$Name_add,
                            Flag=input$Flag_add,
                            Type=input$Type_add,
                            Domain=input$Domain_add,
                            Conditions=input$Conditions_add )
        
        valuesTable$Data<-rbind(valuesTable$Data, new_row )
        removeModal()
        shinyalert(title = "Done", type = "success")
    })
    
    
    
    
    ### save to RDS part 
    observeEvent(input$update_data,{
        saveRDS(valuesTable$Data, "note.rds")
        shinyalert(title = "Data saved", type = "success")
    })
    
    ### delete selected rows part
    ### this is a warning message for deleting
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
                    paste("Please select the parameter to edit" ),easyClose = TRUE
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
            paste("irace parameters", ".txt", sep="\n")
        },
        content = function(file) {
            write.table(data.frame(valuesTable$Data), file, row.names = F, col.names = F, sep="", quote = F)
        }
    )
    
    output$exportOptions<- downloadHandler(
        filename = function() {
            paste("scenario", ".txt")
        },
        content = function(file) {
            write.table(paste("###General Options\n\n",
                              "parameterFile=",input$parameterFile,"\n",
                              "trainInstancesDir=",input$trainInstancesDir,"\n",
                              "trainInstancesFile=",input$trainInstancesFile,"\n",
                              "scenarioFile=",input$scenarioFile,"\n",
                              "execDir= ",input$execDir,"\n",
                              "logFile=",input$logFile,"\n",
                              "debugLevel=",input$debugLevel,"\n",
                              "seed=",as.integer(input$seed),"\n",
                              "repairConfiguration=",input$repairConfiguration,"\n",
                              "postSelection=",input$postSelection,"\n",
                              "aclib=",as.integer(input$aclib),"\n\n",
                              "###Elitist Race\n\n",
                              "elitist=",as.integer(input$elitist),"\n",
                              "elitistLimit=",input$elitistLimit,"\n",
                              "elitistNewInstances=",input$elitistNewInstances,"\n",
                              "###Internal Irace Options\n\n",
                              "nbIterations=",input$nbIterations,"\n\n",
                              "nbExperimentsPerIteration=",input$nbExperimentsPerIteration,"\n",
                              "minNbSurvival=",input$minNbSurvival,"\n",
                              "nbConfigurations=",input$nbConfigurations,"\n",
                              "mu=",input$mu,"\n",
                              "softRestart=",as.integer(input$softRestart),"\n")
                        
                        
                        , file, row.names = FALSE, col.names = FALSE, sep="", quote = F)
        }
    )
    
    observeEvent(input$elitist,{
        if(input$elitist != TRUE){
            shinyjs::disable("elitistLimit")
            shinyjs::disable("elitistNewInstances")
        }else{
            shinyjs::enable("elitistLimit")
            shinyjs::enable("elitistNewInstances")
        }
    })
    
    ### Files Creation and execution of IRACE
    savedata <- observe({
        input$saveData
        input$saveTarget
        
        iraceFile = file.path("../shared")
        
        
        ### if the directory does not exist, it is created
        
        if(!file.exists(iraceFile)){
            dir.create(file.path("../shared"))
            
        }
        
        ### the directory is set in the created folder
        setwd("../shared") 
        
        
        
    })
    
    observeEvent(input$saveData,{
        if(input$saveData>0){
            x <- data.table(valuesTable$Data)
            if(!file.exists(file.path("../shared/parameters.txt"))){
                write.table(x,"../shared/parameters.txt",row.names = FALSE, col.names = FALSE,  sep = '\t', quote = F)
            }
            
            
            if(is.null(input$file1)){
                shinyalert("Please upload instance file",type = "error")
                return()
            }
            write.table(paste(readLines(input$file1$datapath)),"../shared/instances.txt",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
            
            if(is.null(input$file2) && !file.exists(file.path("../shared/target-runner"))) { 
                shinyalert("Please upload target script and save it",type = "error")
                return()
            }
            
            if(file.exists(file.path("../shared/target-runner"))) {
                shinyalert("Files are saved! you can run IRACE",type = "success")
            }
            
            
            if(file.exists(file.path("../shared/parameters.txt")) && file.exists(file.path("../shared/scenario.txt")) && file.exists(file.path("../shared/instances.txt")) && file.exists(file.path("../shared/target-runner"))){
                enable("start")
            }
        }
        
    })
    
    observeEvent(input$saveTarget,{
        if(input$saveTarget>0){
            if(is.null(input$file2)) { shinyalert("Please upload target script and save it",type = "error")
                return()}
            write.table(paste(input$query),"../shared/target-runner",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
            Sys.chmod("../shared/target-runner", "777", use_umask = FALSE)
            shinyalert("Target saved",type = "success")
        }
    })
    observeEvent(input$start,{
        if(input$start>0){
            showModal(
                modalDialog(title = "Warning",
                            paste("Are you sure to start execution of IRACE?"),
                            footer = tagList(
                                modalButton("Cancel"),
                                actionButton("acept_start", "Yes")
                            ), easyClose = TRUE)
                
                
            )
        }
    })
    observeEvent(input$acept_start,{
        if(input$acept_start>0){
            removeModal()
            shinyalert("IRACE is now running", type = "success")
            setwd('../irace_scenario_setup')
            script <- paste0(getwd(), "/runIrace.R")
            system(paste0("Rscript -e 'source(\"", script, "\")'"), wait=FALSE)

            status <- list(goto = 1)
            setwd('../')
            path <- paste0(getwd(), '/shared/acotsp-arena/irace.Rdata')
            assign("pathRDATA", path, envir=.GlobalEnv, inherits = FALSE)
            js$closewindow()
            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            stopApp(returnValue = invisible(status))
        }  
    })
    
    
    ### This section save all the user inputs on the IRACE options
    observeEvent(input$saveOptions,{
        if(input$saveOptions>0 && input$elitist != FALSE){
            write.table(paste("###General Options\n\n",
                              "parameterFile=",input$parameterFile,"\n",
                              "trainInstancesDir=",input$trainInstancesDir,"\n",
                              "trainInstancesFile=",input$trainInstancesFile,"\n",
                              "scenarioFile=",input$scenarioFile,"\n",
                              "execDir= ",input$execDir,"\n",
                              "debugLevel=",input$debugLevel,"\n",
                              "seed=",as.integer(input$seed),"\n",
                              "repairConfiguration=",input$repairConfiguration,"\n",
                              "postSelection=",input$postSelection,"\n",
                              "maxExperiments= 5000","\n",
                              "aclib=",as.integer(input$aclib),"\n\n",
                              "###Elitist Race\n\n",
                              "elitist=",as.integer(input$elitist),"\n",
                              "elitistLimit=",input$elitistLimit,"\n",
                              "elitistNewInstances=",input$elitistNewInstances,"\n",
                              "###Internal Irace Options\n\n",
                              "nbIterations=",input$nbIterations,"\n\n",
                              "nbExperimentsPerIteration=",input$nbExperimentsPerIteration,"\n",
                              "minNbSurvival=",input$minNbSurvival,"\n",
                              "nbConfigurations=",input$nbConfigurations,"\n",
                              "mu=",input$mu,"\n",
                              "softRestart=",as.integer(input$softRestart),"\n"
                              
                              
                              
                              
            ),"../shared/scenario.txt",row.names = FALSE,col.names = FALSE, sep = "",quote = F)
            if(file.exists(file.path("../shared/scenario.txt"))){
                shinyalert("File Saved!")
            }
            
        }
        if(input$saveOptions>0 && input$elitist == FALSE){
            write.table(paste("###General Options\n\n",
                              "parameterFile=",input$parameterFile,"\n",
                              "trainInstancesDir=",input$trainInstancesDir,"\n",
                              "trainInstancesFile=",input$trainInstancesFile,"\n",
                              "scenarioFile=",input$scenarioFile,"\n",
                              "execDir= ",input$execDir,"\n",
                              "logFile=",input$logFile,"\n",
                              "debugLevel=",input$debugLevel,"\n",
                              "seed=",as.integer(input$seed),"\n",
                              "repairConfiguration=",input$repairConfiguration,"\n",
                              "postSelection=",input$postSelection,"\n",
                              "aclib=",as.integer(input$aclib),"\n\n",
                              "###Elitist Race\n\n",
                              "elitist=",as.integer(input$elitist),"\n",
                              "###Internal Irace Options\n\n",
                              "nbIterations=",input$nbIterations,"\n\n",
                              "nbExperimentsPerIteration=",input$nbExperimentsPerIteration,"\n",
                              "minNbSurvival=",input$minNbSurvival,"\n",
                              "nbConfigurations=",input$nbConfigurations,"\n",
                              "mu=",input$mu,"\n",
                              "softRestart=",as.integer(input$softRestart),"\n"
                              
                              
                              
                              
            ),"../shared/scenario.txt",row.names = FALSE,col.names = FALSE, sep = "",quote = F)
            if(file.exists(file.path("../shared/scenario.txt"))){
                shinyalert("File Saved!")
            }
            
        }
    })
    
    
    ### The following lines add the scenario edition
    # observeEvent(input$editOptions,{
    #     ScenarioFileOpt = file.path("../Irace_scenario-setup/scenario.txt")
    #     if(input$editOptions>0 && file.exists(ScenarioFileOpt) ){
    #         showModal(
    #             modalDialog(title = "Scenario.txt", size = c("m", "s", "l"),
    #                         textAreaInput(inputId = "scenarioText","",value = paste(readLines("../Irace_scenario-setup-/scenario.txt"), collapse = "\n"), width = '190%',
    #                                       height = '400px', resize = NULL),
    #                         actionButton("saveEditionOp","Save"),
    #                         easyClose = TRUE
    #             )
    #         )
    #         
    #         
    #         
    #     }
    #     
    # })
    
    
    # observeEvent(input$saveEditionOp,{
    #     if(input$saveEditionOp>0){
    #         write.table(paste(input$scenarioText),"../Irace_scenario-setup/scenario.txt",row.names = FALSE,col.names = FALSE, sep = '', quote = F)
    #     }
    #     shinyalert("File Edited!")
    # 
    # 
    # })
    # 
    
    
    
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

    session$onSessionEnded(function() {
        if(flagStop == FALSE)
        {
            print('SESSION ENDED BY SETUP APP')
            assign("flagStop", TRUE, envir=.GlobalEnv,inherits = FALSE)
            stopApp()
        }
    })
}

