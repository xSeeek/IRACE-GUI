library('irace')

print("Running IRACE...")
setwd('../shared')
parameters = readParameters(file="parameters.txt")
scenario = readScenario(filename="scenario.txt")
irace(scenario=scenario, parameters=parameters)