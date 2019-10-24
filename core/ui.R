addResourcePath(prefix = 'resources', directoryPath = '../resources')

load('../resources/data/example-irace.Rdata', envir=.GlobalEnv)
print("IRACE version of the report: ")
print(iraceResults$irace.version)

bestConfiguration <- getFinalElites(iraceResults, n = 0)[1,]
#print(bestConfiguration)

htmlTemplate("../www/reportes.html",
  iraceVersion = iraceResults$irace.version,
  dataScenario = iraceResults$scenario,
  dataParameters = iraceResults$parameters,
  bestConfiguration = bestConfiguration
)