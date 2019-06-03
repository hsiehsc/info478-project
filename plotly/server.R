
state_list <- state.name
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
                z = ~High.Value, text = ~hover, locations = ~StateAbb,
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
    homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F,
                             check.names = F)
    homicide <- melt(homicide_raw, id = "Year")
    homicide <- na.omit(homicide)
    homicide <- homicide %>%
                rename(State = variable)
    homicide_select <- reactive({homicide %>% 
        filter(State == input$state_text_name)})

    output$homicide_graph <- renderPlotly({
      plot_ly(data = homicide_select(), type = "scatter", x=~Year, y=~value) 
})
    ##### minimum wage and homicide rates #####
    homicide_wage <- left_join(homicide, data)
    homicide_wage <- na.omit(homicide_wage)
    
    hom_wage_select <- reactive({homicide_wage %>% 
        filter(State == input$state_hom_wage)})
    
    output$p_hom_wage <- renderPlotly({
      plot_ly(data = hom_wage_select(), type = "scatter", x=~value,
              y=~High.2018) 
    })
})
