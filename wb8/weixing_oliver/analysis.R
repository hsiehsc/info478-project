library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
read.csv("minimum_wage_data.csv")

HW_WA_CA_minimum_wage <- minimum_wage %>%
                        filter(State == "Hawaii" | State == "Washington" | State == "California")

p <- ggplot(HW_WA_CA_minimum_wage, aes(x= State, y= Low.2018)) + 
      geom_violin(trim = FALSE)
p + stat_summary(fun.y = mean, geom = "point", size = 1, color = "red")
p + geom_boxplot(width=0.1)
ggsave(filename = "Oliver_Nie_data.png")