addResourcePath(prefix = 'resources', directoryPath = '../resources')

junk <- dir(pattern="tempPlot")
file.remove(junk)

htmlTemplate("../www/reportes.html",
  # CORE DATA
  loadReport = fileInput("reportLoader", "Load Report", multiple = FALSE, accept=c('application/x-r-data', '.RData', '.Rdata'), width = 200),

  # SUMMARY PARAMETERS
  bestConfiguration = htmlOutput('bestConfigurationsDetails'),
  boxPlotBestConfiguration = plotOutput("boxPlotBestConfiguration"),

  # DETAILS BY ITERATION PARAMETERS
  selectDetailsIteration = selectInput("iterationDetails", "Iteration:", 1:iraceResults$state$nbIterations),
  bestSoFarSelected = htmlOutput('bestSoFarSelected'),
  eliteConfigurationSelected = htmlOutput('eliteConfigurationSelected'),

  # CANDIDATES PARAMETERS
  selectedParametersCandidates = selectInput("selectedParametersCandidates", "Parameters to be displayed: ", iraceResults$parameters$names, selected = iraceResults$parameters$names, multiple = TRUE, width = 2500, selectize = TRUE),
  frecuencyCandidates = plotOutput("frecuencyCandidates"),
  parallelCoordinatesCandidates = plotOutput("parallelCoordinatesCandidates"),
  sliderCandidates = sliderInput("iterationPlotsCandidates", label = h5("Iteration"), min = 1, max = iraceResults$state$nbIterations, value = c(iraceResults$state$nbIterations - 1, iraceResults$state$nbIterations)),

  # Performance PARAMETERS
  selectIterationPerformance = selectInput("iterationPlotsPerformance", "Iteration:", 1:iraceResults$state$nbIterations, selected = iraceResults$state$nbIterations),
  boxPlotPerformance = plotOutput("boxPlotPerformance"),
  convergencePerformance = plotOutput("convergencePerformance")
)