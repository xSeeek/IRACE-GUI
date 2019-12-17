absolutePath <- getwd()

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

removeTemporalPlots <- function()
{
    junk <- dir(pattern="tempPlot")
    file.remove(junk)
}

generateFrequencyPlot <- function(iterations, parameters)
{
    configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterations[1]:iterations[2])

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
    removeTemporalPlots()
    return(base64plots)
}

generateParallelCoordinatesPlot <- function(iterations, parameters)
{
    last <- length(iraceResults$iterationElites)
    conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = iterations[1]:iterations[2])
    
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
    removeTemporalPlots()
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
    removeTemporalPlots()

    return(results)
}

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
        req(input$iterationDetails)
        bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
        parameters = setupContentTable(iraceResults$parameters$names, '<th>', '</th>');
        bestConfigurationData = setupContentTable(detailsBestConfiguration, '<td>', '</td>');
        meanValue = colMeans(iraceResults$experiments[,iraceResults$iterationElites[as.integer(input$iterationDetails)], drop=FALSE], na.rm=TRUE)

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
        req(input$iterationDetails)
        bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
        dataTable = createTableEliteConfigurations(bestConfigurations[[1]])
        parameters = setupContentTable(iraceResults$parameters$names, '<th>', '</th>');

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
        last <- length(iraceResults$iterationElites)
        id <- paste0(iraceResults$iterationElites[last])
        results <- subset(iraceResults$experiments, select=c(id))
        conf <- gl(ncol(results), nrow(results), labels = colnames(results))
        pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
        configurationsBoxplot (results, ylab = "Solution cost")
    })

    output$boxPlotPerfomance <- renderImage({
        req(input$iterationPlotsPerfomance)
        plot <- generateBoxPlot(input$iterationPlotsPerfomance)
        list(src = plot$dir)
    })

    paramsCandidates <- reactive({
        validate(
            need(length(input$selectedParametersCandidates) >= 2, "At least two parameters are necessary to generate the plots.")
        )
    })

    output$frecuencyCandidates <- renderImage({
        req(input$iterationPlotsCandidates)
        req(input$selectedParametersCandidates)
        paramsCandidates()

        configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = input$iterationPlotsCandidates[1]:input$iterationPlotsCandidates[2])

        max <- 12
        limit <- 1
        params <- c()
        numberOfParameters <- ceiling(length(input$selectedParametersCandidates)/max)
        for(i in 1: numberOfParameters)
        {
            k <- 1
            for(j in limit:(max*i))
            {
                if(length(input$selectedParametersCandidates) >= j)
                {
                    params[k] <- input$selectedParametersCandidates[j]
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
        removeTemporalPlots()
        image_write(finalPlot, path = "../resources/images/frequencyPlot.png", format = "png")
        list(src = "../resources/images/frequencyPlot.png")
    })

    output$parallelCoordinatesCandidates <- renderImage({
        req(input$iterationPlotsCandidates)
        req(input$selectedParametersCandidates)
        paramsCandidates()

        last <- length(iraceResults$iterationElites)
        conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = input$iterationPlotsCandidates[1]:input$iterationPlotsCandidates[2])
        
        max <- 12
        limit <- 1
        params <- c()
        numberOfParameters <- ceiling(length(input$selectedParametersCandidates)/max)
        for(i in 1: numberOfParameters)
        {
            k <- 1
            for(j in limit:(max*i))
            {
                if(length(input$selectedParametersCandidates) >= j)
                {
                    params[k] <- input$selectedParametersCandidates[j]
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
        removeTemporalPlots()
        image_write(finalPlot, path = "../resources/images/parallelPlot.png", format = "png")
        list(src = "../resources/images/parallelPlot.png")
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
        customSections <- input$customSections
        customSectionsNames <- input$customSectionsNames
        customSectionsIDS <- input$customSectionsIDS
        checkIfExists(paste0("../resources/data/", input$reportName, ".RData"), TRUE)
        dir.create("../reports/")
        save(iraceResults, customSections, customSectionsNames, customSectionsIDS, file = paste0("../reports/", input$reportName, ".RData"))
        #encrypt_file(paste0("../resources/data/", input$reportName, ".RData"), outfile = paste0("../resources/data/", input$reportName, ".RData"))
    })

    observeEvent(input$reportLoader, {
        dataToLoad <- input$reportLoader

        if(length(ls(envir=.GlobalEnv, pattern="customSectionsNames")) != 0)
            rm(customSectionsNames, envir = .GlobalEnv)
        if(length(ls(envir=.GlobalEnv, pattern="customSectionsIDS")) != 0)
            rm(customSectionsIDS, envir = .GlobalEnv)
        if(length(ls(envir=.GlobalEnv, pattern="customSections")) != 0)
            rm(customSections, envir = .GlobalEnv)

        removeTemporalPlots()

        stopApp(returnValue = invisible(dataToLoad$datapath))
    }, once = TRUE)

    observeEvent(input$requestPlottingCandidates, {
        req(input$selectedParametersCandidates)
        
        iterations <- input$requestPlottingCandidates
        parameters <- input$selectedParametersCandidates

        frequencyPlot <- generateFrequencyPlot(iterations, parameters, TRUE, FALSE)
        parallelCoordinatesPlot <- generateParallelCoordinatesPlot(iterations, parameters, TRUE, FALSE)
        images <- list(frequency = frequencyPlot$image, parallel = parallelCoordinatesPlot$image)

        session$sendCustomMessage("imagePlotCandidates", images)
    })

    observeEvent(input$requestPlottingPerfomance, {
        iteration <- input$requestPlottingPerfomance;
        boxPlot <- generateBoxPlot(iteration)
        image <- list(boxPlot = boxPlot$image)

        session$sendCustomMessage("imagePlotPerfomance", image)
    })

    observeEvent(input$requestBestSoFarIterations, {
        params <- input$requestBestSoFarIterations
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
    })

    output$bestSoFarTableDetails <- DT::renderDataTable({
        #bestConfiguration <- getConfigurationById(iraceResults, ids=110)
        datatable(iraceResults$allConfigurations)
    })
}