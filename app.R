# import library
library(shiny)

# load server and UI
source("app_server.R")
source("app_ui.R")

# build a Shiny app
shinyApp(ui = my_ui, server = my_server)