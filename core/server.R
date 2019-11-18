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
        formatedData <- paste0(formatedData, '<br>&#10132', colnames(dataToFormat[i]), ': &emsp;', dataToFormat[i])
    return(formatedData)
}

server <- function(input, output, session) {
    resourcesPath <- paste(absolutePath, "/resources", sep = "")

    output$bestConfigurationsDetails <- renderUI({
        last <- length(iraceResults$iterationElites)
        id <- iraceResults$iterationElites[last]
        bestConfiguration <- getConfigurationById(iraceResults, ids = id)
        formatedDataBestConfiguration <- setupContentBestConfiguration(bestConfiguration)

        HTML('<b>&#9819Best-so-far</b>
            <br>&#10132configurarion: &emsp;', bestConfiguration[[1]],
            '<br>&#10132mean value: &emsp;', (colMeans(iraceResults$experiments[,iraceResults$iterationElites[last], drop=FALSE], na.rm=TRUE)[[1]]),
            '<br>&#10132PARENT: &emsp;&emsp;', bestConfiguration[[length(bestConfiguration)]],
            '<br>
            <b>Description of the best-so-far configurarion</b>', formatedDataBestConfiguration)
    })

    output$iterationSelected <- renderUI({
        req(input$iterationDetails)
        bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
        dataTable = createTableEliteConfigurations(bestConfigurations[[1]])
        parameters = setupContentTable(iraceResults$parameters$names, '<th>', '</th>');
        bestConfigurationData = setupContentTable(detailsBestConfiguration, '<td>', '</td>');
        meanValue = colMeans(iraceResults$experiments[,iraceResults$iterationElites[as.integer(input$iterationDetails)], drop=FALSE], na.rm=TRUE)

        HTML('<b>Best-so-far configuration: </b>', bestConfigurationID, '<br><b>mean value: </b>', meanValue[[1]]
        , '<br><br><b>Description of the best-so-far configuration:</b><br>
        <table class="table table-bordered table-sm display" id="best-so-far">
            <thead>
                <tr>
                    <th>ID</th>'
                    , parameters,
                    '<th>PARENT</th>
                </tr> 
            </thead>
            <tbody> 
                <tr>', bestConfigurationData, '</tr>
            </tbody></table>
        <br><b>Elite configurations: </b><br>
        <table class="table table-bordered table-sm display" id="best-so-far">
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
        results <- subset(iraceResults$testing$experiments, select=c(id))
        conf <- gl(ncol(results), nrow(results), labels = colnames(results))
        pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
        configurationsBoxplot (results, ylab = "Solution cost")
    })

    output$boxPlotPerfomance <- renderPlot({
        req(input$iterationPlotsPerfomance)
        configurationPerIteration <- convertVectorToString(iraceResults$allElites[as.integer(input$iterationPlotsPerfomance)][[1]])
        results <- iraceResults$experiments
        intersectedColumns <- formatColData(results, configurationPerIteration)
        results <- subset(iraceResults$experiments, select=(intersectedColumns))
        conf <- gl(ncol(results), nrow(results), labels = colnames(results))
        pairwise.wilcox.test (as.vector(results), conf, paired = TRUE, p.adj = "bonf")
        configurationsBoxplot (results, ylab = "Solution cost")
    })

    output$frecuencyCandidates <- renderPlot({
        req(input$iterationPlotsCandidates)
        configurations <- getConfigurationByIteration(iraceResults = iraceResults, iterations = c(input$iterationPlotsCandidates[1], input$iterationPlotsCandidates[2]))
        parameterFrequency(configurations, iraceResults$parameters)
    })

    output$paralelCoordinatesCandidates <- renderPlot({
        req(input$iterationPlotsCandidates)
        last <- length(iraceResults$iterationElites)
        conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = c(input$iterationPlotsCandidates[1], input$iterationPlotsCandidates[2]))
        parallelCoordinatesPlot (conf, iraceResults$parameters, hierarchy = FALSE)
        #, param_names = c("algorithm", "alpha", "beta", "rho", "q0") -> CHECK
    })

    output$convergencePerfomance <- renderPlot({
        iters <- unique(iraceResults$experimentLog[,"iteration"])
        fes <- cumsum(table(iraceResults$experimentLog[,"iteration"]))
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
}