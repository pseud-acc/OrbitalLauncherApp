#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("superhero"),
    
    tags$head(
        
        tags$style(type="text/css", "#text{ height: 200px; word-wrap: break-word; }"),
        
        tags$style(type="text/css", "#value{ height: 200px; word-wrap: break-word; }")
        
    ),     
    
    navbarPage(

        "Orbital Launch Data App",
        ##########
        ## Page 1
        ##########        
        tabPanel(
            "Home",
            sidebarLayout(
                sidebarPanel(
                    # Check Box for either LEO or GTO orbital launchers
                    radioButtons("LEO_GTO","Select Payload-capability:",
                                 choices = c("LEO","GTO")),
                    # Check Box for Current/Future and Retired/Cancelled Orbital Launchers
                    checkboxGroupInput("current_retired","Select Operational Status:",
                                       choices = c("Current/Future","Retired/Cancelled"),
                                       selected = "Current/Future"),
                    # Slider input to show Orbital Launchers above a minimum number of launches
                    sliderInput("min_launches",
                                "Select Minimum Number of Launches:",
                                min = 0,
                                max = 500,
                                value = 5,),
                    # Check box input to show logarithmic or linear scale on plot
                    radioButtons("log_linear","Plot linear or logarithmic scale:",
                                 choices = c("Linear","Log"),selected = "Log")            
                ),
                mainPanel(
                    
                    tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "
                                    
                                    
                    )),
                    tabsetPanel(type = "tabs",
                                tabPanel("Payload vs. First Flight", br(),plotlyOutput("launchplot",width=900,height=700)),
                                tabPanel("Summary by Country",br(),DT::dataTableOutput('summary'))
                    )
                )
            )
        ),
        
        tabPanel(
            "Documentation",
            mainPanel(htmlOutput('about'))
                
            )
        
    )
))
