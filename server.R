source("maker.R")
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
    return(maker(input$target))
  })
  
  ##### EDUCATION ATTAINMENT ##### 
  
  # Read education data
  edu_df <- read.csv("data/edu/all_edu.csv", stringsAsFactors = F)
  combined_df <- read.csv("data/edu/combined_edu_obese.csv", stringsAsFactors = F)
  
  # Filter Education DF by Year
  select_choro_yr <- 
    reactive({
      filter(edu_df, year == input$shiny_yr) # ui, select year
    })
  
  # Create choropleth map of education attainment:
  # (either by at least bachelors or at least high school grad)
  edu_choro <- reactive({
    plot_ly(
      select_choro_yr(),
      type = "choropleth",
      z = ~ at_least_hs_grad,
      key = ~ state,
      source = "stateplot",
      zmin = 75,
      zmax = 100,
      hoverinfo = "text",
      text = paste0(
        select_choro_yr()$state, ": ",
        select_choro_yr()$at_least_hs_grad, "%" 
      ),
      locations = ~ Abbreviation,
      locationmode = "USA-states",
      color = ~ at_least_hs_grad,
      colors = brewer.pal(9, "YlGnBu"),
      colorbar = list(title = "Percentage")
    ) %>%
      layout(
        title = "Percentage of Residents with At Least A High School Diploma",
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = F
        )
      )
  })
  output$edu_choropleth = renderPlotly(edu_choro())
  
  # Reactive proportion bar by state and year
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "stateplot")
    if (length(s) == 0) {
      cat("Click on a state to display a detailed education attainment plot\n",
          "(double click to clear)")
    } else {
      cat("You selected: ", s$key)
    }
  })
  
  output$state_bar <-
    renderPlotly({
      # Get selected state
      s <- event_data("plotly_click", source = "stateplot")
      if (length(s)) {
        selected_state <- s$key
        # Filter by state and year
        selected_state_yr <- 
          reactive({
            dplyr::filter(edu_df, state == selected_state, year == input$shiny_yr)
          })
        # Make plot
        reactive({
          plot_ly(selected_state_yr(),
                  x = ~less_than_9th,
                  source = "stateplot",
                  type = "bar",
                  name = "Less than 9th grade",
                  orientation = "h",
                  hovertext = paste0("Less than 9th grade: ", selected_state_yr()$less_than_9th, "%"),
                  hoverinfo = "text",
                  marker = list(color = "#ffffcc",
                                line = list(color = "#000000",
                                            width = 1))) %>%
            add_trace(x = ~`X9th_to_12th`,
                      name = "9th to 12th grade",
                      hovertext = paste0("9th to 12th grade: ", selected_state_yr()$`X9th_to_12th`, "%"),
                      marker = list(color = "#c7e9b4")) %>%
            add_trace(x = ~hs_grad,
                      name = "High School Graduate",
                      hovertext = paste0("High School Graduate: ", selected_state_yr()$hs_grad, "%"),
                      marker = list(color = "#7fcdbb")) %>%
            add_trace(x = ~some_col,
                      name = "Some college",
                      hovertext = paste0("Some college: ", selected_state_yr()$some_col, "%"),
                      marker = list(color = "#41b6c4")) %>%
            add_trace(x = ~associates,
                      name = "Associate's Degree",
                      hovertext = paste0("Associate's Degree: ", selected_state_yr()$associates, "%"),
                      marker = list(color = "#1d91c0")) %>%
            add_trace(x = ~bachelors,
                      name = "Bachelor's Degree",
                      hovertext = paste0("Bachelor's Degree: ", selected_state_yr()$bachelors, "%"),
                      marker = list(color = "#225ea8")) %>%
            add_trace(x = ~graduate,
                      name = "Graduate Degree",
                      hovertext = paste0("Graduate Degree: ", selected_state_yr()$graduate, "%"),
                      marker = list(color = "#0c2c84")) %>%
            layout(barmode = 'stack',
                   title = paste("Percentage of Education Attainment in",
                                 selected_state_yr()$state, "in", selected_state_yr()$year),
                   xaxis = list(title="Percentage"),
                   yaxis = list(showticklabels = F),
                   margin = list(pad = 10))
        })()
      } else {
        plotly_empty(
          type = "bar"
        )
      }
    })
  
  # Scatter plot of obesity vs percent > high school (omit DC)
  
  # Select Year
  select_combined_yr <- 
    reactive({
      dplyr::filter(combined_df, year == input$shiny_yr2, geo_id2 != 11) %>% 
        na.omit()
    })
  
  # Find Linear Regression Coefficient
  linear_coeff <- reactive({round(
    cor(select_combined_yr()$x.rfbmi5, select_combined_yr()$at_least_hs_grad), 3)})
  
  obese_edu_plot <- reactive({
    plot_ly(select_combined_yr(),
            x = ~x.rfbmi5,
            y = ~at_least_hs_grad,
            type = "scatter",
            mode = "markers",
            hovertext = paste0(select_combined_yr()$state, ": ", '\n',
                               "Percent Completed At Least High School: ", 
                               select_combined_yr()$at_least_hs_grad, "%", '\n',
                               "Obesity Rate: ", round(select_combined_yr()$x.rfbmi5, 3)),
            hoverinfo = "text",
            marker = list(size = 10,
                          color = 'rgba(51, 102, 204, .9)',
                          line = list(color = 'rgba(0, 0, 102, .8)',
                                      width = 2))) %>%
      layout(xaxis = list(title="Obesity Rate"),
             yaxis = list(title="% Completed At Least High School"),
             title = paste0("% Completed At Least High School vs Obesity Rate in ",
                            select_combined_yr()$year),
             annotations = list(text = paste0("r = ", linear_coeff()),
                                showarrow = F,
                                font = list(color = 'rgba(51, 102, 156, 1)'),
                                x = 0.585,
                                y = 80))
  })
  
  output$obese_edu = renderPlotly(obese_edu_plot())
})
