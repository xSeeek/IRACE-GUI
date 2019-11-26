addResourcePath(prefix = 'resources', directoryPath = '../resources')

if(length(ls(envir=.GlobalEnv, pattern="iraceResults")) == 0)
  load('../resources/data/irace-lingeling.Rdata', envir=.GlobalEnv)

htmlTemplate("../www/reportes.html",
  # CORE DATA
  loadAnotherRdata = fileInput("rdataLoader", "Load ", multiple = FALSE, accept = NULL,
                      width = 200, 
                      placeholder = "Load IRACE results"),

  # SUMMARY PARAMETERS
  bestConfiguration = htmlOutput('bestConfigurationsDetails'),
  boxPlotBestConfiguration = plotOutput("boxPlotBestConfiguration"),

  # DETAILS BY ITERATION PARAMETERS
  selectDetailsIteration = selectInput("iterationDetails", "Iteration:", 1:iraceResults$state$nbIterations),
  selectedIteration = htmlOutput('iterationSelected'),

  # CANDIDATES PARAMETERS
  selectedParametersCandidates = selectInput("selectedParametersCandidates", "Parameters to be displayed: ", iraceResults$parameters$names, selected = iraceResults$parameters$names, multiple = TRUE, width = 600, size = 5, selectize = FALSE),
  frecuencyCandidates = plotOutput("frecuencyCandidates"),
  paralelCoordinatesCandidates = plotOutput("paralelCoordinatesCandidates"),
  sliderCandidates = sliderInput("iterationPlotsCandidates", label = h5("Iteration"), min = 1, max = iraceResults$state$nbIterations, value = c(iraceResults$state$nbIterations - 1, iraceResults$state$nbIterations)),

  # PERFOMANCE PARAMETERS
  selectIterationPerfomance = selectInput("iterationPlotsPerfomance", "Iteration:", 1:iraceResults$state$nbIterations, selected = iraceResults$state$nbIterations),
  boxPlotPerfomance = plotOutput("boxPlotPerfomance"),
  convergencePerfomance = plotOutput("convergencePerfomance")
)