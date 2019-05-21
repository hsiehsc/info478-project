library(tidyverse)
library(ggplot2)

data <- read.csv("minimum_wage_data.csv")

California <- data %>% 
  filter(State == "California")

graph <- ggplot(California) + geom_line(aes(x=Year, y=CPI.Average)) + labs(title="CPI Average change in Years", y = "CPI Average")
