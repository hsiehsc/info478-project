library(plotly)
library(tidyverse)

###### Minimum Wage Choropleth #####


data <- read.csv("data/minimum_wage_data.csv", stringsAsFactors = F)

data_select <- data %>% 
  filter(Year == "2017")

data_select$StateAbb <- state.abb[match(data_select$State, state.name)]

data_select$hover <- with(data_select, paste(State, '<br>',
                                             "High Value:", High.Value,
                                    "<br>", "2018 Adj Low Value:", Low.2018,
                                    '<br>', "2018 Adj High Value:", High.2018
                                    ))

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

##### Minimum Wage State Choropleth Plotly #####
p <- plot_geo(data_select, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Low.Value, text = ~hover, locations = ~StateAbb,
    color = ~Low.Value, colors = 'Greens'
  ) %>%
  colorbar(title = "Minimum Wage") %>%
  layout(
    title = 'Minimum Wage by State<br>(Hover for breakdown)',
    geo = g
  )

##### Homicide Data Work #####
state_list <- state.name
homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F)
homicide <- homicide_raw[-52:-218,]
homicide_select <- homicide %>% 
                              select(Year, California)
output$homicide_graph <- reactive(renderPlotly({
  plot_ly(data = homicide_select, x=~Year, y=~California)
}))


plot_ly(data = homicide_wa, x=~Year, y=~Washington)