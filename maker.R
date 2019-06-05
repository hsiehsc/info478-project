library("shiny")
library(plotly)
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(tidyr)



maker <- function(target) {

  final_data <- left_join(minimum_wage, all_data, by = c("State" = "X", "Year" = "Year"))
  
  if(!is.null(target) & !("All" %in% target)) {
    final_data <- final_data[final_data$State %in% target, ]
  } 
  
  final_data <- final_data %>%
    select(State, Year, x.rfbmi2, High.2018)
  names(final_data)[3] <- "Percentage"
  names(final_data)[4] <- "minimum Wage"
  
  
  minimum_year_overweight_obesity_plot <- plot_ly(final_data, x = ~Year, y = ~State, 
                                                  z = ~`minimum Wage`, color = ~Percentage) %>%
    layout(title = "3D plot of Obesity and overweight VS Year VS Minimum wage") %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Years (2001 --- 2017'),
                        yaxis = list(title = 'States'),
                        zaxis = list(title = 'Minimum wage in state ($)')))
  minimum_year_overweight_obesity_plot 
}