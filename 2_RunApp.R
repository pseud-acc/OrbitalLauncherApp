#load packages
library(shiny)
library(plotly)

#load launcher data
launchers <- readRDS("data/launchers.rds")

#run shiny App
runApp("ShinyApp")