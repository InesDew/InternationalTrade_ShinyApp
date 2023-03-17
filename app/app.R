# Load packages ----------------------------------------------------------------
library(shiny)


shinyApp(ui = shiny::source("ui.R", local = TRUE)$value,
         server = shiny::source("server.R", local = TRUE)$value)
