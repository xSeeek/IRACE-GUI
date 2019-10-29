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
  iraceVersion = iraceResults$irace.version,
  dataScenario = iraceResults$scenario,
  dataState = iraceResults$state,
  dataParameters = iraceResults$parameters,
  bestConfiguration = bestConfiguration,
  selectIteration = selectInput("iteration", "Iteration:", choices),
  selectedIteration = htmlOutput('iterationSelected'),
  boxPlotBestConfiguration = plotOutput("boxPlotBestConfiguration"),
  frecuencyParameters = plotOutput("frecuencyParameters"),
  evaluatedAlgorithms = formatedText,
  paralelCoordinatesCandidates = plotOutput("paralelCoordinatesCandidates"),
  sliderCandidates = sliderInput("iterationPlotsCandidates", label = h5("Iteration"), min = 1, max = iraceResults$state$nbIterations, value = c(iraceResults$state$nbIterations - 1, iraceResults$state$nbIterations))
)