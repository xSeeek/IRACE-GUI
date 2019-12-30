addResourcePath(prefix = 'resources', directoryPath = '../reports/resources')

htmlTemplate("../www/index.html",
  # CORE DATA
  loadReport = fileInput("reportLoader", "Open Scenario / Load Rdata", multiple = FALSE, accept=c('application/x-r-data', '.RData', '.Rdata'), width = 200, placeholder = "Load")
)