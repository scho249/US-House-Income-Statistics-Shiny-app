library("shiny")
source("my_ui.R")
library(rsconnect)

shinyApp(ui = my_ui, server = my_server)
