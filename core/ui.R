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

htmlTemplate("../www/reportes.html",
  iraceVersion = iraceResults$irace.version,
  dataScenario = iraceResults$scenario,
  dataState = iraceResults$state,
  dataParameters = iraceResults$parameters,
  bestConfiguration = bestConfiguration,
  selectIteration = selectInput("iteration", "Iteration:", choices),
  selectedIteration = htmlOutput('iterationSelected')
)