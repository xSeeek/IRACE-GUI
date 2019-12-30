createTableEliteConfigurations <- function(configurationsData)
{
    dataTable = c()
    for(i in 1:length(configurationsData))
    {
        valueData <- ""
        actualConfiguration <- getConfigurationById(iraceResults, ids = configurationsData[i])
        for(j in 1:(length(actualConfiguration) - 1))
            valueData <- paste0(valueData, '<td>', actualConfiguration[j], '</td>')
        dataTable[i] = paste0('<tr>', valueData,'</tr>')
    }
    formatedResults = createOneLine(dataTable)
    return(formatedResults)
}

createOneLine <- function(tableVector)
{
    fullLine = ""
    for(i in 1:length(tableVector))
        fullLine = paste0(fullLine, tableVector[i])
    return(fullLine)
}

convertVectorToString <- function(vector)
{
    newVector = c()
    for(i in 1:length(vector))
        newVector[i] <- paste0(vector[i])
    return(newVector)
}

formatColData <- function(resultsData, iterationData)
{
    vectorColNames <- colnames(resultsData)
    formatedData <- Reduce(intersect, list(vectorColNames, iterationData))
    return(formatedData)
}

setupContentTable <- function(toFormat, open, close)
{
    data <- toFormat
    formatedData <- ""
    for(i in 1:length(data))
        formatedData <- paste0(formatedData, open, data[[i]], close)
    return(formatedData)
}

setupContentBestConfiguration <- function(dataToFormat)
{
    formatedData <- ""
    for(i in 2:(length(dataToFormat)-1))
    {
        formatedData <- paste(formatedData, '<br>&#10132', colnames(dataToFormat[i]), ': &emsp;', dataToFormat[i], sep = "", collapse = NULL)
    }
    return(formatedData)
}

checkIfExists <- function(fileName, flagDelete)
{
    if(file.exists(fileName))
        if(flagDelete)
        {
           unlink(fileName) 
           return(TRUE);
        }
    return(FALSE)
}

removeTemporalPlots <- function(patternData)
{
    junk <- dir(pattern=patternData)
    file.remove(junk)
}

generateFrequencyPlot <- function(iterations, parameters)
{
    configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = as.integer(iterations[1]):as.integer(iterations[2]))

    max <- 12
    limit <- 1
    params <- c()
    numberOfParameters <- ceiling(length(parameters)/max)
    base64plots <- c();
    for(i in 1: numberOfParameters)
    {
        k <- 1
        for(j in limit:(max*i))
        {
            if(length(parameters) >= j)
            {
                params[k] <- parameters[j]
                k <- k + 1
            }
        }

        # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
        fixFormat <- iraceResults$parameters
        fixFormat$names <- params

        png(filename <- paste0("tempPlotFrequency", i, ".png"), width = 1500, height = 1500, res = 200)
        parameterFrequency(configurations, fixFormat)
        dev.off()

        base64image <- base64Encode(readBin(filename, "raw", file.info(filename)[1, "size"]), "txt")
        base64image <- paste0('data:image/png;base64,', base64image)
        base64plots[i] <- base64image

        limit <- (max*i) + 1;
    }
    removeTemporalPlots('tempPlotFrequency')
    return(base64plots)
}

generateParallelCoordinatesPlot <- function(iterations, parameters)
{
    last <- length(iraceResults$iterationElites)
    conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = as.integer(iterations[1]):as.integer(iterations[2]))
    
    max <- 12
    limit <- 1
    params <- c()
    numberOfParameters <- ceiling(length(parameters)/max)
    base64plots <- c();
    for(i in 1: numberOfParameters)
    {
        k <- 1
        for(j in limit:(max*i))
        {
            if(length(parameters) >= j)
            {
                params[k] <- parameters[j]
                k <- k + 1
            }
        }

        # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
        fixFormat <- iraceResults$parameters
        fixFormat$names <- params

        png(filename <- paste0("tempPlotParallel", i, ".png"), width = 1500, height = 1500, res = 200)
        parallelCoordinatesPlot (conf, fixFormat, hierarchy = FALSE)
        dev.off()

        base64image <- base64Encode(readBin(filename, "raw", file.info(filename)[1, "size"]), "txt")
        base64image <- paste0('data:image/png;base64,', base64image)
        base64plots[i] <- base64image

        limit <- (max*i) + 1;
    }
    removeTemporalPlots('tempPlotParallel')
    return(base64plots)
}

generateBoxPlot <- function(numberIteration)
{
    configurationPerIteration <- convertVectorToString(iraceResults$allElites[as.integer(numberIteration)][[1]])
    results <- iraceResults$experiments
    intersectedColumns <- formatColData(results, configurationPerIteration)
    results <- subset(iraceResults$experiments, select=(intersectedColumns))
    conf <- gl(ncol(results), nrow(results), labels = colnames(results))
    pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")

    png(filename <- paste0("tempPlotBoxplot.png"), width = 2000, height = 3000, res = 400)
    configurationsBoxplot(results, ylab = "Solution cost")
    dev.off()

    base64image <- base64Encode(readBin(filename, "raw", file.info(filename)[1, "size"]), "txt")
    base64image <- paste0('data:image/png;base64,', base64image)

    plot <- image_read("tempPlotBoxplot.png")
    plot <- image_scale(plot, "x750")
    image_write(plot, path = "../resources/images/boxPlot.png", format = "png")
    results <- list(dir = '../resources/images/boxPlot.png', image = base64image)
    removeTemporalPlots('tempPlotBoxplot')

    return(results)
}

plan(multiprocess)

server <- function(input, output, session) {    
    if(length(ls(envir=.GlobalEnv, pattern="loadedCustomSection")) == 1)
    {
        if(length(ls(envir=.GlobalEnv, pattern="customSectionsNames")) == 0)
            customSectionsNames <- NULL
        if(length(ls(envir=.GlobalEnv, pattern="customSectionsIDS")) == 0)
            customSectionsIDS <- NULL
        if(length(ls(envir=.GlobalEnv, pattern="customSections")) == 0)
            customSections <- NULL

        dataSelect <- list(names = customSectionsNames, ids = customSectionsIDS, content = customSections)
        session$sendCustomMessage("loadCustomSections", dataSelect)
    }

    session$onSessionEnded(function() {
        print('SESSION ENDED BY REPORTS APP')
        if(recentlyLoadedReports == FALSE)
        {
            status <- list(goto = -1)
            session$sendCustomMessage(type = "closeWindow", message = "message")
            stopApp(returnValue = invisible(status))
        }
        assign("recentlyLoadedReports", FALSE, envir=.GlobalEnv,inherits = FALSE)
    })

    output$bestConfigurationsDetails <- renderUI({
        last <- length(iraceResults$iterationElites)
        id <- iraceResults$iterationElites[last]
        bestConfiguration <- getConfigurationById(iraceResults, ids = id)
        formatedDataBestConfiguration <- setupContentBestConfiguration(bestConfiguration)

        HTML('<b>&#9819Best-so-far</b>
            <br>&#10132configuration: &emsp;', bestConfiguration[[1]],
            '<br>&#10132mean value: &emsp;', (colMeans(iraceResults$experiments[,iraceResults$iterationElites[last], drop=FALSE], na.rm=TRUE)[[1]]),
            '<br>&#10132PARENT: &emsp;&emsp;', bestConfiguration[[length(bestConfiguration)]],
            '<br>&#10132Total instances tested: &emsp;', sum(!is.na(iraceResults$experiments[ ,iraceResults$iterationElites[length(iraceResults$iterationElites)] ])),
            '<br>
            <br><b>Description of the best-so-far configuration</b>', formatedDataBestConfiguration)
    })

    output$bestSoFarSelected <- renderUI({
        withProgress(message = 'Generating table: Best-so-far', value = 0, {
            incProgress(1/10, detail = paste("Preconfiguring..."))
            req(input$iterationDetails)
            incProgress(2/10, detail = paste("Obtaining data..."))
            bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
            bestConfigurationID <- bestConfigurations[[1]][1]
            detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
            incProgress(6/10, detail = paste("Configuring table..."))
            parameters = setupContentTable(iraceResults$parameters$names, '<th>', '</th>');
            bestConfigurationData = setupContentTable(detailsBestConfiguration, '<td>', '</td>');
            meanValue = colMeans(iraceResults$experiments[,iraceResults$iterationElites[as.integer(input$iterationDetails)], drop=FALSE], na.rm=TRUE)
            incProgress(10/10, detail = paste("Finishing..."))
        })
        HTML('<b>Best-so-far configuration: </b>', bestConfigurationID, '<br><b>mean value: </b>', meanValue[[1]]
        , '<br><br><b>Description of the best-so-far configuration:</b><br>
        <table class="table table-bordered table-sm display" id="bestSoFarIteration">
            <thead>
                <tr>
                    <th>ID</th>'
                    , parameters,
                    '<th>PARENT</th>
                </tr> 
            </thead>
            <tbody> 
                <tr>', bestConfigurationData, '</tr>
            </tbody>
        </table>
        <br>'
        )
    })

    output$eliteConfigurationSelected <- renderUI({
        withProgress(message = 'Generating table: Elite configurations', value = 0, {
            incProgress(1/10, detail = paste("Preconfiguring..."))
            req(input$iterationDetails)
            incProgress(2/10, detail = paste("Obtaining data..."))
            bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
            bestConfigurationID <- bestConfigurations[[1]][1]
            detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
            incProgress(6/10, detail = paste("Configuring table..."))
            dataTable = createTableEliteConfigurations(bestConfigurations[[1]])
            parameters = setupContentTable(iraceResults$parameters$names, '<th>', '</th>');
            incProgress(10/10, detail = paste("Finishing..."))
        })

        HTML('<br><b>Elite configurations: </b><br>
        <table class="table table-bordered table-sm display" id="bestSoFarConfigsIteration">
            <thead>
                <tr>
                    <th>ID</th>'
                    , parameters,
                '</tr> 
            </thead>
            <tbody>'
                , dataTable,
            '</tbody>
        </table>'
        )
    })

    output$boxPlotBestConfiguration <- renderPlot({
        withProgress(message = 'Plotting: Boxplot Perfomance', value = 0, {
            incProgress(1/10, detail = paste("Preconfiguring..."))
            last <- length(iraceResults$iterationElites)
            id <- paste0(iraceResults$iterationElites[last])
            results <- subset(iraceResults$experiments, select=c(id))
            conf <- gl(ncol(results), nrow(results), labels = colnames(results))
            incProgress(2/10, detail = paste("Rendering plots..."))
            pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
            incProgress(10/10, detail = paste("Finishing..."))
        })
        configurationsBoxplot (results, ylab = "Solution cost")
    })

    output$boxPlotPerfomance <- renderImage({
        withProgress(message = 'Plotting: Boxplot Perfomance', value = 0, {
            incProgress(1/10, detail = paste("Preconfiguring..."))
            req(input$iterationPlotsPerfomance)
            incProgress(2/10, detail = paste("Rendering plots..."))
            plot <- generateBoxPlot(input$iterationPlotsPerfomance)
            incProgress(10/10, detail = paste("Finishing..."))
        })
        list(src = plot$dir)
    })

    paramsCandidates <- reactive({
        validate(
            need(length(paramsCand()) >= 2, "ERROR: At least two parameters are necessary to generate the plots.")
        )
    })

    paramsInput <- reactive({
        input$selectedParametersCandidates
    })

    itersInput <- reactive({
        input$iterationPlotsCandidates
    })

    paramsCand <- debounce(paramsInput, 2500)
    itersCand <- debounce(itersInput, 1000)

    output$frecuencyCandidates <- renderImage({
        paramsCandidates()
        parameters <- paramsCand()
        iterations <- itersCand()

        progress <- AsyncProgress$new(message = 'Plotting: Frequency Plot', detail = 'This may take a while...', value = 0)
        progress$inc(1/10, detail = paste("Preconfiguring..."))
        req(iterations)

        configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterations[1]:iterations[2])
        max <- 12
        limit <- 1
        params <- c()
        numberOfParameters <- ceiling(length(parameters)/max)
        progress$inc(2/10, detail = paste("Rendering plots..."))
        future({
            for(i in 1: numberOfParameters)
            {
                k <- 1
                for(j in limit:(max*i))
                {
                    if(length(parameters) >= j)
                    {
                        params[k] <- parameters[j]
                        k <- k + 1
                    }
                }

                # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
                fixFormat <- iraceResults$parameters
                fixFormat$names <- params

                png(filename = paste0("tempPlotFrequency", i, ".png"), width = 550, height = 555, res = 80)
                parameterFrequency(configurations, fixFormat)
                dev.off()
                limit <- (max*i) + 1;
            }
        }) %...>%(function(result) {
            progress$inc(6/10, detail = paste("Concatenating images..."))
            finalPlot <- NULL
            for(i in 1:numberOfParameters)
            {
                if(is.null(finalPlot))
                {
                    finalPlot <- image_read(paste0("tempPlotFrequency", i, ".png"))
                    next
                }
                image <- image_read(paste0("tempPlotFrequency", i, ".png"))
                finalPlot <- image_append(c(finalPlot, image), stack = TRUE)
            }
            image_write(finalPlot, path = "../resources/images/frequencyPlot.png", format = "png")
        })%...>% (function(result) {
            progress$inc(9/10, detail = paste("Finishing..."))
            list(src = "../resources/images/frequencyPlot.png")
        })%>%finally(function(){
            progress$inc(10/10, detail = paste("Removing temporal plots..."))
            removeTemporalPlots('tempPlotFrequency')
            progress$close()
        })
    })

    output$parallelCoordinatesCandidates <- renderImage({
        paramsCandidates()
        parameters <- paramsCand()
        iterations <- itersCand()

        progress <- AsyncProgress$new(message = 'Plotting: Parallel Coordinates', detail = 'This may take a while...', value = 0)
        progress$inc(1/10, detail = paste("Preconfiguring..."))
        req(iterations)

        last <- length(iraceResults$iterationElites)
        conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterations[1]:iterations[2])
        
        max <- 12
        limit <- 1
        params <- c()
        numberOfParameters <- ceiling(length(parameters)/max)
        progress$inc(2/10, detail = paste("Rendering plots..."))
        future({
            for(i in 1: numberOfParameters)
            {
                k <- 1
                for(j in limit:(max*i))
                {
                    if(length(parameters) >= j)
                    {
                        params[k] <- parameters[j]
                        k <- k + 1
                    }
                }

                # TEMPORAL FIX DUE IMPLEMENTATION OF THE PLOT
                fixFormat <- iraceResults$parameters
                fixFormat$names <- params

                png(filename = paste0("tempPlotParallel", i, ".png"))
                parallelCoordinatesPlot (conf, fixFormat, hierarchy = FALSE)
                dev.off()
                limit <- (max*i) + 1;
            }
        }) %...>%(function(result) {
            progress$inc(6/10, detail = paste("Concatenating images..."))
            finalPlot <- NULL
            for(i in 1:numberOfParameters)
            {
                if(is.null(finalPlot))
                {
                    finalPlot <- image_read(paste0("tempPlotParallel", i, ".png"))
                    next
                }
                image <- image_read(paste0("tempPlotParallel", i, ".png"))
                finalPlot <- image_append(c(finalPlot, image), stack = TRUE)
            }
            image_write(finalPlot, path = "../resources/images/parallelPlot.png", format = "png")
        })%...>% (function(result) {
            progress$inc(9/10, detail = paste("Finishing..."))
            list(src = "../resources/images/parallelPlot.png")
        })%>%finally(function(){
            progress$inc(10/10, detail = paste("Removing temporal plots..."))
            removeTemporalPlots('tempPlotParallel')
            progress$close()
        })
    })

    output$convergencePerfomance <- renderPlot({
        iters <- unique(iraceResults$experimentLog[,"iteration"])
        fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
        fes <- fes[!names(fes) == '0']
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

    observeEvent(input$customSections, {
        customSections <- input$customSections$arrayOfSections
        customSectionsNames <- input$customSectionsNames
        customSectionsIDS <- input$customSectionsIDS
        checkIfExists(paste0("../resources/data/", input$reportName, ".RData"), TRUE)
        save(iraceResults, customSections, customSectionsNames, customSectionsIDS, file = paste0("../../saved/", input$reportName, ".RData"))
    })

    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader

        if(dataToLoad$type != 'application/x-r-data')
        {
            session$sendCustomMessage(type = "invalidFiletype", message = "message")
            return(NULL)
        }

        if(length(ls(envir=.GlobalEnv, pattern="customSectionsNames")) != 0)
            rm(customSectionsNames, envir = .GlobalEnv)
        if(length(ls(envir=.GlobalEnv, pattern="customSectionsIDS")) != 0)
            rm(customSectionsIDS, envir = .GlobalEnv)
        if(length(ls(envir=.GlobalEnv, pattern="customSections")) != 0)
            rm(customSections, envir = .GlobalEnv)

        removeTemporalPlots('tempPlot')

        session$sendCustomMessage(type = "closeWindow", message = "message")
        status <- list(goto = 2, path = dataToLoad$datapath)
        stopApp(returnValue = invisible(status))
    }, once = FALSE)

    observeEvent(input$backMainMenu, {
        status <- list(goto = 0)
        assign("recentlyLoadedReports", TRUE, envir=.GlobalEnv,inherits = FALSE)
        session$sendCustomMessage(type = "closeWindow", message = "message")
        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    observeEvent(input$launchSetup, {
        status <- list(goto = 1)

        assign("recentlyLoadedReports", TRUE, envir=.GlobalEnv,inherits = FALSE)
        session$sendCustomMessage(type = "closeWindow", message = "message")
        stopApp(returnValue = invisible(status))
    }, once = TRUE)

    observeEvent(input$requestPlottingCandidates, {
        req(input$selectedParametersCandidates)
        print(input$requestPlottingCandidates$iterations)
        
        iterations <- input$requestPlottingCandidates$iterations
        parameters <- input$selectedParametersCandidates

        frequencyPlot <- generateFrequencyPlot(iterations, parameters)
        parallelCoordinatesPlot <- generateParallelCoordinatesPlot(iterations, parameters)
        images <- list(frequency = frequencyPlot, parallel = parallelCoordinatesPlot)

        session$sendCustomMessage("imagePlotCandidates", images)
        return(NULL)
    }, once = FALSE)

    observeEvent(input$requestPlottingPerfomance, {
        iteration <- input$requestPlottingPerfomance$iterations;
        boxPlot <- generateBoxPlot(iteration)
        image <- list(boxPlot = boxPlot$image)

        session$sendCustomMessage("imagePlotPerfomance", image)
        return(NULL)
    }, once = FALSE)

    observeEvent(input$requestBestSoFarIterations, {
        params <- unlist(input$requestBestSoFarIterations$params)
        bestSoFarIterations <- list()

        bestSoFarIterations[[1]] <- params
        for(i in 1:iraceResults$state$nbIterations)
        {
            bestConfigurations <- iraceResults$allElites[as.integer(i)]
            bestConfiguration <- bestConfigurations[[1]][1]
            detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfiguration)
            detailsBestConfiguration <- subset(detailsBestConfiguration, select = c('.ID.', params, '.PARENT.') )

            meanValue <- colMeans(iraceResults$experiments[,iraceResults$iterationElites[as.integer(i)], drop=FALSE], na.rm=TRUE)

            buildData <- list(id = bestConfiguration, mean = as.numeric(meanValue), paramData = detailsBestConfiguration)
            bestSoFarIterations[[i + 1]] <- buildData
        }
        session$sendCustomMessage("bestSoFarAllIterations", bestSoFarIterations)
        return(NULL)
    }, once = FALSE)
}