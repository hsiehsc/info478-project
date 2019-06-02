library(shiny)
library(plotly)
library(tidyverse)

shinyServer(function(input, output) {
    ### plotly choropleth ####
    output$plotlymap <- renderPlotly({
        data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)
        data_select <- data %>% 
            filter(Year == input$year_slider)
        data_select$hover <- with(data_select, paste(State, '<br>',
                                                     "High Value:", High.Value,
                                                     "<br>", "2018 Adj Low Value:", Low.2018,
                                                     '<br>', "2018 Adj High Value:", High.2018
        ))
        data_select$StateAbb <- state.abb[match(data_select$State, state.name)]
        # give state boundaries a white border
        l <- list(color = toRGB("white"), width = 2)
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = FALSE,
            lakecolor = toRGB('white')
        )
        plot_geo(data_select, locationmode = 'USA-states') %>%
            add_trace(
                z = ~Low.Value, text = ~hover, locations = ~StateAbb,
                color = ~Low.Value, colors = 'Greens'
            ) %>%
            colorbar(title = "Minimum Wage") %>%
            layout(
                title = 'Minimum Wage by State<br>(Hover for breakdown)',
                geo = g
            )
        
})
    ##### end plotly choropleth #####
    
    ##### homicide #####
    state_list <- state.name
    homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F)
    homicide <- homicide_raw[-52:-218,]
    homicide_select <- reactive(homicide %>% 
        select(Year, input$state_text_name))
    output$homicide_graph <- renderPlotly({
        plot_ly(data = homicide_select(), x=~Year, y=~input$state_text_name)
    })
})

