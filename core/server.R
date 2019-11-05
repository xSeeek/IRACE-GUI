absolutePath <- getwd()

#iraceResults[["parameters"]][["names"]]

createTableEliteConfigurations <- function(configurationsData)
{
    dataTable = c()
    for(i in 1:length(configurationsData))
    {
        actualConfiguration <- getConfigurationById(iraceResults, ids = configurationsData[i])
        dataTable[i] = paste('
            <tr><td>', actualConfiguration$.ID., '</td>
            <td>', actualConfiguration$algorithm, '</td>
            <td>', actualConfiguration$localsearch, '</td>
            <td>', actualConfiguration$alpha, '</td>
            <td>', actualConfiguration$beta, '</td>
            <td>', actualConfiguration$rho, '</td>
            <td>', actualConfiguration$ants, '</td>
            <td>', actualConfiguration$nnls, '</td>
            <td>', actualConfiguration$q0, '</td>
            <td>', actualConfiguration$dlb, '</td>
            <td>', actualConfiguration$rasrank, '</td>
            <td>', actualConfiguration$elitistants, '</td></tr>
        ', sep = "")
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

server <- function(input, output, session) {
    resourcesPath <- paste(absolutePath, "/resources", sep = "")

    output$iterationSelected <- renderUI({
        req(input$iterationDetails)
        bestConfigurations <- iraceResults$allElites[as.integer(input$iterationDetails)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
        dataTable = createTableEliteConfigurations(bestConfigurations[[1]])

        HTML('<b>Best-so-far configuration: </b>', bestConfigurationID, '<br><b>mean value: </b>', input$iterationDetails
        , '<br><br><b>Description of the best-so-far configuration:</b><br>
        <table class="table table-bordered table-sm" id="best-so-far">
            <thead>
                <tr>
                    <th>ID</th>
                    <th>algorithm</th>
                    <th>localsearch</th>
                    <th>alpha</th>
                    <th>beta</th>
                    <th>rho</th>
                    <th>ants</th>
                    <th>nnls</th>
                    <th>q0</th>
                    <th>dlb</th>
                    <th>rasrank</th>
                    <th>elitistants</th>
                    <th>PARENT</th>
                </tr> 
            </thead>
            <tbody> 
                <tr>
                    <td>', detailsBestConfiguration$.ID., '</td>
                    <td>', detailsBestConfiguration$algorithm, '</td>
                    <td>', detailsBestConfiguration$localsearch, '</td>
                    <td>', detailsBestConfiguration$alpha, '</td>
                    <td>', detailsBestConfiguration$beta, '</td>
                    <td>', detailsBestConfiguration$rho, '</td>
                    <td>', detailsBestConfiguration$ants, '</td>
                    <td>', detailsBestConfiguration$nnls, '</td>
                    <td>', detailsBestConfiguration$q0, '</td>
                    <td>', detailsBestConfiguration$dlb, '</td>
                    <td>', detailsBestConfiguration$rasrank, '</td>
                    <td>', detailsBestConfiguration$elitistants, '</td>
                    <td>', detailsBestConfiguration$.PARENT, '</td>
                </tr>
            </tbody></table>
        <br><b>Elite configurations: </b><br>
        <table class="table table-bordered table-sm" id="best-so-far">
            <thead>
                <tr>
                    <th>ID</th>
                    <th>algorithm</th>
                    <th>localsearch</th>
                    <th>alpha</th>
                    <th>beta</th>
                    <th>rho</th>
                    <th>ants</th>
                    <th>nnls</th>
                    <th>q0</th>
                    <th>dlb</th>
                    <th>rasrank</th>
                    <th>elitistants</th>
                </tr> 
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

    output$frecuencyParameters <- renderPlot({
        parameterFrequency(iraceResults$allConfigurations, iraceResults$parameters)
    })

    output$paralelCoordinatesCandidates <- renderPlot({
        req(input$iterationPlotsCandidates)
        last <- length(iraceResults$iterationElites)
        conf <- getConfigurationByIteration(iraceResults = iraceResults, iterations = c(input$iterationPlotsCandidates[1], input$iterationPlotsCandidates[2]))
        parallelCoordinatesPlot (conf, iraceResults$parameters, param_names = c("algorithm", "alpha", "beta", "rho", "q0"), hierarchy = FALSE)
    })
}