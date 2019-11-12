addResourcePath(prefix = 'resources', directoryPath = '../resources')

load('../resources/data/example-irace.Rdata', envir=.GlobalEnv)
print("IRACE version of the report: ")
print(iraceResults$irace.version)

last <- length(iraceResults$iterationElites)
id <- iraceResults$iterationElites[last]
bestConfiguration <- getConfigurationById(iraceResults, ids = id)

choices = c()
for (i in 1:iraceResults$state$nbIterations) 
  choices[i] <- i

eAlgorithms <- unique(iraceResults$allConfigurations$algorithm)
formatedText <- "Evaluated algorithms: "
for(i in 1:length(eAlgorithms))
  formatedText <- paste0(formatedText, " ", eAlgorithms[i])

htmlTemplate("../www/reportes.html",
  # GENERAL DATA
  evaluatedAlgorithms = formatedText,

  # SUMMARY PARAMETERS
  iraceVersion = iraceResults$irace.version,
  dataScenario = iraceResults$scenario,
  dataState = iraceResults$state,
  dataParameters = iraceResults$parameters,
  bestConfiguration = htmlOutput('bestConfigurationsDetails'),
  boxPlotBestConfiguration = plotOutput("boxPlotBestConfiguration"),

  # DETAILS BY ITERATION PARAMETERS
  selectDetailsIteration = selectInput("iterationDetails", "Iteration:", choices),
  selectedIteration = htmlOutput('iterationSelected'),

  # CANDIDATES PARAMETERS
  frecuencyCandidates = plotOutput("frecuencyCandidates"),
  paralelCoordinatesCandidates = plotOutput("paralelCoordinatesCandidates"),
  sliderCandidates = sliderInput("iterationPlotsCandidates", label = h5("Iteration"), min = 1, max = iraceResults$state$nbIterations, value = c(iraceResults$state$nbIterations - 1, iraceResults$state$nbIterations)),

  # PERFOMANCE PARAMETERS
  selectIterationPerfomance = selectInput("iterationPlotsPerfomance", "Iteration:", choices, selected = iraceResults$state$nbIterations),
  boxPlotPerfomance = plotOutput("boxPlotPerfomance"),
  convergencePerfomance = plotOutput("convergencePerfomance")
)