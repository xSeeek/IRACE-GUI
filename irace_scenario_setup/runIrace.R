library('irace')

print("Running IRACE...")
setwd('../Irace_scenario-setup')
parameters = readParameters(file="parameters.txt")
scenario = readScenario(filename="scenario.txt")
irace(scenario=scenario, parameters=parameters)