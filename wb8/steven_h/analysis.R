library(tidyverse)
library(plotly)

data <- read.csv("minimum_wage_data.csv", stringsAsFactors = F)

washington_data <- data %>% 
  filter(State == "Washington")


p <- plot_ly(data = washington_data, x = ~Year, y =~Low.2018) %>% 
  layout(title = paste0("Minimum wage in Washington over Time", 
                        " converted to 2018 dollars"), yaxis = list(
                          title = "Adjusted Minimum Wage"
                        ))

plot_ly(data = washington_data, x = ~CPI.Average, y =~Low.2018) %>% 
  layout(title = "CPI in Washington State Over Time")

orca(p, file = "min_wa_wage_adjusted.png")


