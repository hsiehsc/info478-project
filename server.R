library(shiny)
library(leaflet)
library(tidyverse)
library(tigris)

shinyServer(function(input, output) {
    output$leafletmap <- renderLeaflet({
        states <- states()
        
        data <- read.csv("data/minimum_wage_data.csv", stringsAsFactors = F)
        
        data_2017 <- data %>% 
            filter(Year == input$year_slider)
        
        states_merged <- geo_join(states, data_2017, "NAME", "State")
        
        pal <- colorNumeric("Greens", domain=states_merged$Low.Value)
        
        states_merged <- subset(states_merged, !is.na(Low.Value))
        
        popup <- paste0("<strong>", states_merged$NAME, 
                        "</strong><br />Total: ", states_merged$Low.Value)
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-98.483330, 38.712046, zoom = 4) %>% 
            addPolygons(data = states_merged , 
                        fillColor = ~pal(states_merged$Low.Value), 
                        fillOpacity = 0.7, 
                        weight = 0.2, 
                        smoothFactor = 0.2, 
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label=popup,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addLegend(pal = pal, 
                      values = states_merged$Low.Value, 
                      position = "bottomright", 
                      title = "Minimum Wage ($)")
    })
})
