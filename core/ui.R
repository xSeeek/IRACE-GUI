addResourcePath(prefix = 'resources', directoryPath = '../resources')

load('../resources/data/example-irace.Rdata', envir=.GlobalEnv)
print("IRACE version of the report: ")

#summary <- c(iraceResults$irace.version,
#            iraceResults$scenario)
#print(summary[3])

htmlTemplate("../www/reportes.html",
  iraceVersion = iraceResults$irace.version,
  dataScenario = iraceResults$scenario,
  dataParameters = iraceResults$parameters
)