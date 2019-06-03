library(plotly)
library(tidyverse)
library(tidyr)
library(reshape2)

###### Minimum Wage Choropleth #####


data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)

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
homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F,
                         check.names=F)
test <- melt(homicide_raw, id = "Year")
test <- na.omit(test)
test <- rename(test, State = variable)

test_select <- test %>% 
  filter(State == "New Hampshire")

data_select <- data %>% 
  filter(State == "New Hampshire")

test_data <- left_join(data_select, test_select)
plot_ly(data = test_data, type = "scatter", x=~High.2018, y=~value) %>% 
  layout(title = "aasdf", xaxis = list(title = "asdf"))



homicide <- homicide_raw[-52:-218,]
homicide <- homicide[,-53:-56]
homicide_select <- homicide %>% 
                              select(Year, one_of("New York"))
output$homicide_graph <- reactive(renderPlotly({
  plot_ly(data = test, type = "scatter", x=~Year, y=) 
}))


plot_ly(data = homicide_select, x=~Year, y=~California)

##### Homicide Data and Minimum Wage #####
state_list <- state.name
data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)
data_select <- data %>% 
  filter(State == "North Dakota")

homicide_transpose <- read.csv("data/homicide_rates.csv",
                               stringsAsFactors = F)

homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F,
                         check.names = F)
homicide <- melt(homicide_raw, id = "Year")
homicide <- na.omit(homicide)
homicide <- homicide %>%
  rename(State = variable)

homicide_select <- homicide %>% 
    filter(State == "New York")



homicide <- homicide_raw[-52:-218,]
homicide <- homicide[,-53:-56]
homicide_select <- homicide %>% 
  select(Year, one_of("New York"))
output$homicide_graph <- reactive(renderPlotly({
  plot_ly(data = homicide_select, type = "scatter", x=~Year, y=~value) 
}))


