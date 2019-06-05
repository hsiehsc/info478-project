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
  edu_min_wage <- read.csv("data/min_wage_and_edu.csv", stringsAsFactors = F) %>%
    mutate(edu_div10 = at_least_hs_grad / 10,
           ratio = at_least_hs_grad * 0.8 / High.2018)
  obese_min_wage <- read.csv("data/obese_min_wage.csv", stringsAsFactors = F) %>%
    mutate(minwage_div10 = High.2018 / 10,
           ratio = High.2018 / (20 * x.rfbmi5))
  
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
  
  # Obesity Choropleth
  obese_choro_yr <- 
    reactive({
      dplyr::filter(obese_min_wage, year == input$obese_yr)
    })
  
  obese_choropleth <- 
    reactive({
      plot_ly(
        obese_choro_yr(),
        type = "choropleth",
        key = ~ state,
        z = ~ x.rfbmi5,
        zmin = 0.5,
        zmax = 0.75,
        source = "obeseplot",
        hoverinfo = "text",
        text = paste0(
          obese_choro_yr()$state, ": \n", "Obesity Rate: ",
          round(obese_choro_yr()$x.rfbmi5, 3)
        ),
        locations = ~ Abbreviation,
        locationmode = "USA-states",
        color = ~ x.rfbmi5,
        colors = rev(brewer.pal(9, "Spectral")),
        colorbar = list(title = "Obesity Rate")
      ) %>%
      layout(
        title = "Obesity Rate", # ui
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = F
        )
      )
    })
  
  output$obese_choro <- renderPlotly(obese_choropleth())
  
  # Click reactive obese choro
  
  # Click selection for obese choro
  output$selection2 <- renderPrint({
    s <- event_data("plotly_click", source = "obeseplot")
    if (length(s) == 0) {
      cat("Click on a state to display a minimum wage - obesity rate graph\n",
          "(double click to clear)")
    } else {
      cat("You selected: ", s$key)
    }
  })
  
  output$obese_line <- 
    renderPlotly({
      # Get state
      s <- event_data("plotly_click", source = "obeseplot")
      if (length(s)) {
        selected_state <- s$key
        # Filter by state
        select_state_obese <- 
          reactive({
            dplyr::filter(obese_min_wage, state == selected_state)
          })
        # Make Plot
        reactive({
          plot_ly(
            select_state_obese(),
            x = ~year,
            y = ~x.rfbmi5, 
            name = "Obesity Rate",
            type = "scatter",
            mode = "lines",
            line = list(color = "#004080"),
            hoverinfo = "text",
            hovertext = paste0(select_state_obese()$year, ':\n', 
                               "% Completed High School: ", 
                               select_state_obese()$x.rfbmi5, "%")
          ) %>%
            add_trace(
              y = ~minwage_div10,
              name = "Minimum Wage ($/hr)",
              line = list(color = "#008040"),
              hovertext = paste0(select_state_obese()$year, ':\n', "Minimum Wage: $", 
                                 select_state_obese()$High.2018, 
                                 "/hr")
            ) %>%
            add_trace(
              y = ~ratio,
              name = "Adjusted Ratio (Trendline)",
              line = list(color = "#800040"),
              hoverinfo = "none"
            ) %>%
            layout(
              xaxis = list(title = "Year"),
              yaxis = list(showticklabels = F, title = ""),
              title = list(title = paste0("Obesity Rate and Minimum Wage Over Time In ",
                                          select_state_obese()$state))
            )
        })()
      } else {
        plotly_empty(
          type = "scatter"
        )
      }
    })
  
  # Reactive proportion bar by state and year
  
  # Click selection for edu attainment choro
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
  
  # Reactive line graph of state by year
  output$state_line_edu <-
    renderPlotly({
      # Get state
      s <- event_data("plotly_click", source = "stateplot")
      if (length(s)) {
        selected_state <- s$key
        # Filter by state
        select_state_edumw <- 
          reactive({
            dplyr::filter(edu_min_wage, state == selected_state)
          })
        # Make Plot
        reactive({
          plot_ly(
            select_state_edumw(),
            x = ~year,
            y = ~edu_div10, 
            name = "% Completed High School",
            type = "scatter",
            mode = "lines",
            line = list(color = "#004080"),
            hoverinfo = "text",
            hovertext = paste0(select_state_edumw()$year, ':\n', 
                               "% Completed High School: ", 
                               select_state_edumw()$at_least_hs_grad, "%")
          ) %>%
            add_trace(
              y = ~High.2018,
              name = "Minimum Wage ($/hr)",
              line = list(color = "#008040"),
              hovertext = paste0(select_state_edumw()$year, ':\n', "Minimum Wage: $", 
                                 select_state_edumw()$High.2018, 
                                 "/hr")
            ) %>%
            add_trace(
              y = ~ratio,
              name = "Adjusted Ratio (Trendline)",
              line = list(color = "#800040"),
              hoverinfo = "none"
            ) %>%
            layout(
              xaxis = list(title = "Year"),
              yaxis = list(showticklabels = F, title = ""),
              title = list(title = paste0("% Completed High School and Minimum Wage Over Time In ",
                                          select_state_edumw()$state))
            )
        })()
      } else {
        plotly_empty(
          type = "scatter"
        )
      }
    })
})
