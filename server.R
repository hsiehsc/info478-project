state_list <- state.name
data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)
minimum_wage <- data
homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F,
                         check.names = F)
all_data <- read.csv("data/alldata.csv", stringsAsFactors = F)
all_data <- all_data %>%
  filter(x.state != 11)
all_data$X <- state.name

minimum_wage <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)
minimum_wage <- minimum_wage %>%
  filter(Year >= 2001)
minimum_wage <- minimum_wage %>%
  filter(State != "District of Columbia")
minimum_wage <- minimum_wage %>%
  filter(State != "Federal (FLSA)" & State != "Guam" & 
           State != "Puerto Rico" & State != "U.S. Virgin Islands")

shinyServer(function(input, output) {
    ### plotly choropleth ####
    output$plotlymap <- renderPlotly({
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
    homicide <- melt(homicide_raw, id = "Year")
    homicide <- na.omit(homicide)
    homicide <- homicide %>%
                rename(State = variable)
    homicide_select <- reactive({homicide %>%
        filter(State == input$state_text_name)})

    output$homicide_graph <- renderPlotly({
      plot_ly(data = homicide_select(), type = "scatter", x=~Year, y=~value) %>%
        layout(title = paste0("Homicide Rates per 100,000 people in ",
                              input$state_text_name),
               xaxis = list(title = "Year"), yaxis = list(title =
                                      "Homicide Rates per 100,000"))
})
    ##### minimum wage and homicide rates #####
    homicide_wage <- left_join(homicide, data)
    homicide_wage <- na.omit(homicide_wage)
    
    hom_wage_select <- reactive({homicide_wage %>% 
        filter(State == input$state_hom_wage)})
    
    x_hom_wage <- "Minimum Wage in U.S. Dollars (2018 Adjusted)"
    y_hom_wage <- "Homicide Rates per 100,000 people"
    output$p_hom_wage <- renderPlotly({
      plot_ly(data = hom_wage_select(), type = "scatter", x=~High.2018,
              y=~value) %>% 
        layout(xaxis = list(title =
                "Minimum Wage in U.S. Dollars (2018 Adjusted)"),
               yaxis = list(title =
                "Homicide Rates per 100,000 people"),
               title = paste0("Minimum Wage (2018 Adjusted) vs. Homicide",
                              " rates for ", input$state_hom_wage))
    })

    
    final_data <- left_join(minimum_wage, all_data, by = c("State" = "X", "Year" = "Year"))
    final_data <- final_data %>%
      select(State, Year, x.rfbmi2, High.2018)
    names(final_data)[3] <- "Percentage"
    names(final_data)[4] <- "minimum Wage"
#    if(!is.null(input$target)) {
      reactive(final_data <- final_data[final_data$State %in% input$target, ])
#    }
    
    
    output$statee <- renderPlotly({
      plot_ly(final_data, x = ~Year, y = ~State, z = ~`minimum Wage`, color = ~Percentage) %>%
      layout(title = "3D plot of Obesity and overweight VS Year VS Minimum wage") %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Years (2001 --- 2017'),
                          yaxis = list(title = 'States'),
                          zaxis = list(title = 'Minimum wage in state ($)')))
})
})