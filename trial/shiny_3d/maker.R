library("shiny")
library(plotly)
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(snakecase)
library(tidyr)

maker <- function(target) {
  all_data <- read.csv("alldata.csv")
  all_data <- all_data %>%
    filter(x.state != 11)
  ## no columbia district
  all_data$X <- state.name
  minimum_wage <- read.csv("prepped_minimum_wage.csv")
  minimum_wage <- minimum_wage %>%
    filter(Year >= 2001)
  ## take out columbia district
  minimum_wage <- minimum_wage %>%
    filter(State != "District of Columbia")
  minimum_wage <- minimum_wage %>%
    filter(State != "Federal (FLSA)" & State != "Guam" & 
             State != "Puerto Rico" & State != "U.S. Virgin Islands")
  
  final_data <- left_join(minimum_wage, all_data, by = c("State" = "X", "Year" = "Year"))
  final_data <- final_data %>%
    select(State, Year, x.rfbmi2, High.2018)
  names(final_data)[3] <- "Percentage"
  names(final_data)[4] <- "minimum Wage"
  
  #final_data <- subset(final_data, State %in% target)
  
  minimum_year_overweight_obesity_plot <- plot_ly(final_data, x = ~Year, y = ~State, 
                                                  z = ~`minimum Wage`, color = ~Percentage) %>%
    layout(title = "3D plot of Obesity and overweight VS Year VS Minimum wage") %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Years (2001 --- 2017'),
                        yaxis = list(title = 'States'),
                        zaxis = list(title = 'Minimum wage in state ($)')))
  minimum_year_overweight_obesity_plot
}