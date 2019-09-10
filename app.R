library(shiny)
library(shinythemes)
options(shiny.port = 3000)

server <- function(input, output) {
}

shinyApp(ui = htmlTemplate("www/index.html"), server)