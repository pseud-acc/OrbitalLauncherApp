#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

url <- "https://pseud-acc.github.io/OrbitalLauncherApp/data/launchers.rds"
launchers <- readRDS(gzcon(url(url)))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #Information Page
    output$about <- renderUI({
        HTML(
            ' <p><span style="font-weight:bold">About</span></p>
<pstyle="text-align:justify">This web application allows the user to explore data on past/current/future orbital launchers capable of launching 
payloads into LEO (Low-Earth Orbit) and/or GTO (Geostationary-Transfer Orbit).</p>

<p><span style="font-weight:bold">Data</span></p>
All data shown is sourced from the wikipedia page - <a href="https://en.wikipedia.org/wiki/Comparison_of_orbital_launch_systems">Comparison of orbital launch systems</a>. </b>
Information on the vehicle, manufacturer, origin, payload, first flight, latest flight and number of launches is shown (note: orbital launchers are shown only if the first flight has been recorded or scheduled).
</p>

<p><span style="font-weight:bold">Functionality</span></p>

<u>First tab</u>: Plot of Payload vs. First Flight
</p>
<u>Second tab</u>: Summary table of orbital launchers and total launches by country 
</p>
In the left-hand panel the user is able to:
<li> Show either LEO or GTO payload capable orbital launchers in the plot and summary table </li>
<li> Show orbital launchers that are operational/in-development (current/Future) or retired/cancelled in the plot and summary table </li>
<li> Show orbital launchers with a specified minimum number of launches (recorded or scheduled) in the plot and summary table </li>
<li> Show payload data on a linear or logarithmic scale in the payload vs. first flight plot </li>
'            
        )
    })
    
    #create dataset for plotting 
    plot_data <- reactive({
        input_LEO <- ifelse(input$LEO_GTO == "LEO",TRUE,FALSE)
        launchers %>%   mutate(Status = mapvalues(retired, from = c(1,0),
                                                    to = c("Retired/Cancelled","Current/Future"))) %>%
            filter(LEO.capable %in% c(1,input_LEO*1),
                   GTO.capable %in% c(1,(!input_LEO)*1),
                   Status %in% input$current_retired,
                   launches >= input$min_launches) %>%
            mutate(payload = input_LEO*LEO.payload + (!input_LEO)*GTO.payload,
                   Retired = factor(ifelse(retired == 1, "Yes", "No")))
    })
    
    #compute line of best fit
    best_fit <- reactive({    
    bestfit_df <- data.frame()
    for (i in unique(plot_data()$Status)){
        newdata <- data.frame(first.flight = seq(min(plot_data()$first.flight),
                                                 max(plot_data()$first.flight),1),
                              Status = c(i)) 
        model_lm <- glm(payload ~ first.flight, data = plot_data() %>% filter(Status==i),
                        family = Gamma(link="log"))
        newdata$payload <- predict(model_lm, newdata = newdata, type = "response")
        bestfit_df <- rbind(bestfit_df,newdata)
    }
    bestfit_df
    })
    
    #generate summary of data by country
    output$summary <- DT::renderDataTable({
        tbl_summary <- plot_data() %>% dplyr::mutate(Origin=origin) %>% dplyr::group_by(Origin) %>% 
            dplyr::summarise(`Operational/In-Development Orbital Launchers` = sum(ifelse(retired==1,0,1)),
                             `Retired Orbital Launchers` = sum(ifelse(retired==1,1,0)),
                             `Total Number of Launches` = sum(launches),
                             `First Flight` = min(first.flight, na.rm = TRUE),
                             `Latest Flight` = max(latest.flight, na.rm = TRUE)) %>% 
            dplyr::arrange(desc(`Total Number of Launches`))
       tbl_summary
    })    
    
    #generate plot of payload vs. first flight 
    output$launchplot <- renderPlotly({
        ylabel <- paste0(input$LEO_GTO," payload (kg)")
        scale <- ifelse(input$log_linear == "Log","log10","identity")   
        
        xmin <- floor(min(plot_data()$first.flight)/10)*10
        xmax <- ceiling(max(plot_data()$first.flight)/10)*10
        xticks <- seq(xmin,xmax,10)
        xlimits <- c(xmin,xmax)
        
        plot_data2 <- plot_data() %>% mutate(point_labels = paste0("Vehicle = ",vehicle,
                                                                 " <br />Origin = ",origin,
                                                                 " <br />Manufacturer = ",manufacturer,
                                                                 " <br />Number of Launches = ",launches,
                                                                 " <br />", input$LEO_GTO, " Payload (kg) = ",payload,
                                                                 " <br />First Flight = ",first.flight,
                                                                 " <br />Latest Flight = ",latest.flight))
        
        best_fit2 <- best_fit() %>% mutate(point_labels = paste0("Vehicle = N/A",
                                                                   " <br />Origin = N/A",
                                                                   " <br />Manufacturer = N/A",
                                                                   " <br />Number of Launches = N/A",
                                                                   " <br />",input$LEO_GTO," Payload (kg) =",round(payload,2),
                                                                   " <br />First Flight =",first.flight,
                                                                   " <br />Latest Flight = N/A"))
        

        if(length(input$current_retired) > 1){
          p <-  ggplot(data = plot_data2, 
                   aes(x = first.flight , y = payload, colour = Status, text = point_labels)) + 
                geom_point() + scale_y_continuous(trans=scale) +
                xlab("First Flight") + ylab(ylabel) +
                labs(title="Solid line: exponential fit of payload vs. first flight ") +
                geom_line(data = best_fit2, aes(x = first.flight, y = payload, group = Status, colour = Status))+
                theme_bw() +
                theme(axis.line = element_line(colour = "black"),
                      panel.background = element_blank(),
                      plot.title = element_text(face = "italic", colour = "grey", size = 8)) +
                scale_x_continuous(breaks = xticks, limits = xlimits)
        } else {
          p <-  ggplot(data = plot_data2, 
                   aes(x = first.flight , y = payload, text = point_labels)) + 
                geom_point() + scale_y_continuous(trans=scale) +
                xlab("First Flight") + ylab(ylabel) +
                labs(title="Solid line: exponential fit of payload vs. first flight ") +
                geom_line(data = best_fit2, aes(x = first.flight, y = payload, group = 1))+
                theme_bw() +
                theme(axis.line = element_line(colour = "black"),
                      panel.background = element_blank(),
                      plot.title = element_text(face = "italic", colour = "grey", size = 8)) +
                scale_x_continuous(breaks = xticks, limits = xlimits) 
        } 
        
        plotly::ggplotly(p, tooltip="text")
        
    })

})
