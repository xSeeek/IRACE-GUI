addResourcePath(prefix = 'resources', directoryPath = '../resources')

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "black"

junk <- dir(pattern="tempPlot")
file.remove(junk)

htmlTemplate("../www/reportes.html",
  # CORE DATA
  skin = skin,
  loadReport = fileInput("reportLoader", "Load Report", multiple = FALSE, accept = c("application/x-r-data", ".Rdata"), width = 200, placeholder = "Load"),

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

  # PERFOMANCE PARAMETERS
  selectIterationPerfomance = selectInput("iterationPlotsPerfomance", "Iteration:", 1:iraceResults$state$nbIterations, selected = iraceResults$state$nbIterations),
  boxPlotPerfomance = plotOutput("boxPlotPerfomance"),
  convergencePerfomance = plotOutput("convergencePerfomance")
)