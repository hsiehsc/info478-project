library(dplyr)
library(plotly)
library(RColorBrewer)

# Read education data
edu_df <- read.csv("data/edu/all_edu.csv", stringsAsFactors = F)
combined_df <- read.csv("data/edu/combined_edu_obese.csv", stringsAsFactors = F)

shinyServer(function(input, output, session) {
  
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
  
  # Reactive line graph of state by year
  output$state_line_edu <-
    renderPlotly({
      # Get state
      s <- event_data("plotly_click", source = "stateplot")
      if (length(s)) {
        selected_state <- s$key
        # Filter by state
        selected_state_edumw <- 
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