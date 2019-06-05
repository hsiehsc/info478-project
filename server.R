shinyServer(function(input, output) {
  ### plotly choropleth ####
  output$plotlymap <- renderPlotly({
    # Setting filter that matches with the UI
    data_select <- data %>%
      filter(Year == input$year_slider)
    # Hover information for the Plotly choropleth
    data_select$hover <- with(data_select, paste(
      State, "<br>",
      "Low Value:", Low.Value,
      "<br>", "2018 Adj Low Value:", Low.2018,
      "<br>", "2018 Adj High Value:", High.2018
    ))
    # Changing full State names to just their abbreviation
    data_select$StateAbb <- state.abb[match(data_select$State, state.name)]
    # Specify map projection options
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showlakes = FALSE,
      lakecolor = toRGB("white")
    )
    # Creating Choropleth with the main color of green
    plot_geo(data_select, locationmode = "USA-states") %>%
      add_trace(
        z = ~High.Value, text = ~hover, locations = ~StateAbb,
        color = ~Low.Value, colors = "Greens"
      ) %>%
      colorbar(title = "Minimum Wage") %>%
      layout(
        title = "Minimum Wage by State<br>(Hover for breakdown)",
        geo = g
      )
  })
  ##### end plotly choropleth #####

  ##### homicide #####
  # Melting homicide dataframe to better match minimum wage format
  homicide <- melt(homicide_raw, id = "Year")
  # Omitting NA values that were created
  homicide <- na.omit(homicide)
  # Rename column so that homicide and minimum wage can be joined
  homicide <- homicide %>%
    rename(State = variable)
  # Filtering Homicide by the UI input
  homicide_select <- reactive({
    homicide %>%
      filter(State == input$state_text_name)
  })

  output$homicide_graph <- renderPlotly({
    # Creating Plotly for Homicide
    plot_ly(
      data = homicide_select(),
      type = "scatter", x = ~Year, y = ~value
    ) %>%
      layout(
        title = paste0(
          "Homicide Rates per 100,000 people in ",
          input$state_text_name
        ),
        xaxis = list(title = "Year"), yaxis = list(
          title =
            "Homicide Rates per 100,000"
        )
      )
  })
  ##### minimum wage and homicide rates #####
  # Combining Homicide Wage and Minimum Wage
  homicide_wage <- left_join(homicide, data)
  homicide_wage <- na.omit(homicide_wage)

  # Filtering based on the UI
  hom_wage_select <- reactive({
    homicide_wage %>%
      filter(State == input$state_text_name)
  })

  # Creating Plotly
  output$p_hom_wage <- renderPlotly({
    plot_ly(
      data = hom_wage_select(), type = "scatter", x = ~High.2018,
      y = ~value
    ) %>%
      layout(
        xaxis = list(
          title =
            "Minimum Wage in U.S. Dollars (2018 Adjusted)"
        ),
        yaxis = list(
          title =
            "Homicide Rates per 100,000 people"
        ),
        title = paste0(
          "Minimum Wage (2018 Adjusted) vs. Homicide",
          " rates in ", input$state_text_name
        )
      )
  })

  ## Combine obesity data with minimum wage data
  final_data <- left_join(minimum_wage, all_data,
                          by = c("State" = "X", "Year" = "Year"))
  final_data <- final_data %>%
    select(State, Year, x.rfbmi2, High.2018)
  names(final_data)[3] <- "Overweight Percentage"
  names(final_data)[4] <- "minimum Wage"
  reactive(final_data <- final_data[final_data$State %in% input$target, ])
  
  ## Render the 3D plot
  output$statee <- renderPlotly({
    plot_ly(final_data,
      x = ~Year, y = ~State, z = ~`minimum Wage`,
      color = ~`Overweight Percentage`
    ) %>%
    layout(title = "3D plot of Obesity/Overweight vs Year vs Minimum wage") %>%
      add_markers() %>%
      layout(scene = list(
        xaxis = list(title = "Years (2001 --- 2017)"),
        yaxis = list(title = "States"),
        zaxis = list(title = "Minimum wage in state ($)")
      ))
  })
})
