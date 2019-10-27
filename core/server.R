absolutePath <- getwd()

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

server <- function(input, output, session) {
    resourcesPath <- paste(absolutePath, "/resources", sep = "")

    output$iterationSelected <- renderUI({
        req(input$iteration)
        bestConfigurations <- iraceResults$allElites[as.integer(input$iteration)]
        bestConfigurationID <- bestConfigurations[[1]][1]
        detailsBestConfiguration <- getConfigurationById(iraceResults, ids = bestConfigurationID)
        dataTable = createTableEliteConfigurations(bestConfigurations[[1]])

        HTML('Best-so-far configuration: ', bestConfigurationID, '<br>mean value: ', input$iteration
        , '<br><br>Description of the best-so-far configuration:<br>
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
        <br>Elite configurations: <br>
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
            '</tbody></table>'
        )
    })
}