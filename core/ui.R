htmlTemplate("../www/index.html",
  # CORE DATA
  loadReport = fileInput("reportLoader", "Open Scenario", multiple = FALSE, accept = c("application/x-r-data", ".Rdata"), width = 200, placeholder = "Load")
)