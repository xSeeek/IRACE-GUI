addResourcePath(prefix = 'resources', directoryPath = '../resources')

load('../resources/irace.Rdata', envir=.GlobalEnv)

print("IRACE version of the report: ")

htmlTemplate("../www/control.html",
             iraceVersion = iraceResults$irace.version,
             dataScenario = iraceResults$scenario,
             dataParameters = iraceResults$parameters
)