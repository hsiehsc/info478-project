# jerry edu attainment

library(dplyr)
library(plotly)
library(RColorBrewer)

# TO DO: 
#- choropleth of percentage > high school and percentage > bachelors
#- bar plot of percentages
#- percentage high school and percentage bachelors vs obesity

# Choropleth maps:

edu_df <- read.csv("data/edu/all_edu.csv", stringsAsFactors = F)
combined_df <- read.csv("data/edu/combined_edu_obese.csv", stringsAsFactors = F)


select_choro_yr <- filter(edu_df, year == 2009) # ui

# choro bachelor complete
bachelor_choropleth <-
  plot_ly(
    select_choro_yr,
    type = "choropleth",
    z = ~ at_least_bachelors, # ~ui
    zmin = 15,
    zmax = 50,
    hoverinfo = "text",
    text = paste0(
      select_choro_yr$state, ": ", # ~ui
      select_choro_yr$at_least_bachelors, "%"
    ), # ~ui
    locations = ~ Abbreviation,
    locationmode = "USA-states",
    color = ~ at_least_bachelors,
    colors = brewer.pal(9, "YlGnBu"),
    colorbar = list(title = "Percentage")
  ) %>%
  layout(
    title = "Percentage of Residents with At Least A Bachelor's Degree", # ui
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = F
    )
  )

bachelor_choropleth

# choro of % high school complete
hs_choropleth <- 
  plot_ly(
    select_choro_yr,
    type = "choropleth",
    z = ~ at_least_hs_grad,
    zmin = 75,
    zmax = 100,
    hoverinfo = "text",
    text = paste0(
      select_choro_yr$state, ": ",
      select_choro_yr$at_least_hs_grad, "%"
    ),
    locations = ~ Abbreviation,
    locationmode = "USA-states",
    color = ~ at_least_hs_grad,
    colors = brewer.pal(9, "YlGnBu"),
    colorbar = list(title = "Percentage")
  ) %>%
  layout(
    title = "Percentage of Residents with At Least A High School Diploma", # ui
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = F
    )
  )

hs_choropleth

# Horizontal Bar Plot of state and percentages of education attainment
selected_state_yr <- filter(edu_df, state == "Alabama", year == 2009) # ui

state_bar_plot <- 
  plot_ly(selected_state_yr,
          y = ~state,
          x = ~less_than_9th,
          type = "bar",
          name = "Less than 9th grade",
          orientation = "h",
          hovertext = paste0("Less than 9th grade: ", selected_state_yr$less_than_9th, "%"),
          hoverinfo = "text",
          marker = list(color = "#ffffcc",
                        line = list(color = "#000000",
                                    width = 1))) %>%
  add_trace(x = ~`9th_to_12th`,
            name = "9th to 12th grade",
            hovertext = paste0("9th to 12th grade: ", selected_state_yr$`9th_to_12th`, "%"),
            marker = list(color = "#c7e9b4")) %>%
  add_trace(x = ~hs_grad,
            name = "High School Graduate",
            hovertext = paste0("High School Graduate: ", selected_state_yr$hs_grad, "%"),
            marker = list(color = "#7fcdbb")) %>%
  add_trace(x = ~some_col,
            name = "Some college",
            hovertext = paste0("Some college: ", selected_state_yr$some_col, "%"),
            marker = list(color = "#41b6c4")) %>%
  add_trace(x = ~associates,
            name = "Associate's Degree",
            hovertext = paste0("Associate's Degree: ", selected_state_yr$associates, "%"),
            marker = list(color = "#1d91c0")) %>%
  add_trace(x = ~bachelors,
            name = "Bachelor's Degree",
            hovertext = paste0("Bachelor's Degree: ", selected_state_yr$bachelors, "%"),
            marker = list(color = "#225ea8")) %>%
  add_trace(x = ~graduate,
            name = "Graduate Degree",
            hovertext = paste0("Graduate Degree: ", selected_state_yr$graduate, "%"),
            marker = list(color = "#0c2c84")) %>%
  layout(barmode = 'stack',
         title = paste("Percentage of Education Attainment in",
                        selected_state_yr$state, "in", selected_state_yr$year),
         yaxis = list(title=""), 
         xaxis = list(title="Percentage"),
         margin = list(pad = 10))

state_bar_plot  



# Scatter plot of obesity vs percent > high school (omit DC)
select_combined_yr <- filter(combined_df, year == 2009, geo_id2 != 11) %>% 
  na.omit() # ui
linear_coeff <- round(
  cor(select_combined_yr$x.rfbmi5, select_combined_yr$at_least_hs_grad), 3)

obese_edu_plot <-
  plot_ly(select_combined_yr,
          x = ~x.rfbmi5,
          y = ~at_least_hs_grad,
          type = "scatter",
          mode = "markers",
          hovertext = paste0(select_combined_yr$state, ": ", '\n',
                             "Percent Completed At Least High School: ", 
                             select_combined_yr$at_least_hs_grad, "%", '\n',
                             "Obesity Rate: ", round(select_combined_yr$x.rfbmi5, 3)),
          hoverinfo = "text",
          marker = list(size = 10,
                        color = 'rgba(51, 102, 204, .9)',
                        line = list(color = 'rgba(0, 0, 102, .8)',
                                    width = 2))) %>%
  layout(xaxis = list(title="Obesity Rate"),
         yaxis = list(title="% Completed At Least High School"),
         title = paste0("% Completed At Least High School vs Obesity Rate in ",
                        select_combined_yr$year),
         annotations = list(text = paste0("r = ", linear_coeff),
                            showarrow = F,
                            font = list(color = 'rgba(51, 102, 156, 1)'),
                            x = 0.585,
                            y = 80))

obese_edu_plot

write.csv(edu_df, "data/edu/all_edu.csv")
write.csv(combined_df, "data/edu/combined_edu_obese.csv")
