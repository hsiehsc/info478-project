# Jerry Lin WB8
library(dplyr)
library(ggplot2)

df <- read.csv("C:/Users/Jerry/Desktop/info_478/info478-project/data/homicide_rates.csv",
               stringsAsFactors = F)

wa_data <- select(df, Year, Washington) %>% na.omit()

wa_plot <- ggplot(wa_data, aes(Year, Washington, group = 1)) +
  geom_line() +
  labs(y = 'Aggravated Assault Rate', title = 'Aggravated Assault Rate by Year in Washington State')

ggsave("C:/Users/Jerry/Desktop/info_478/info478-project/wb8/jerry_lin/homicide_chart.png",
       wa_plot,
       height = 12,
       width = 24)
