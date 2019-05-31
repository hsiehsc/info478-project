library(dplyr)
library(knitr)
library(ggplot2)
library(tidyr)
library(dplyr)

data <- read.csv("data/minimum_wage_data.csv")

changed <- data %>%
  mutate(High.Value = replace(High.Value, (High.Value < 7.25 & Year >= 2010), 7.25)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 6.55 & Year == 2009), 6.55)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 5.85 & Year == 2008), 5.85)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 5.15 & Year <= 2007 & Year >= 1998), 5.15)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 4.75 & Year == 1997), 4.75)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 4.25 & Year <= 1996 & Year >= 1992), 4.25)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 3.8 & Year == 1991), 3.8)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 3.35 & Year <= 1990 & Year >= 1981), 3.35)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 3.10 & Year == 1980), 3.10)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 2.90 & Year == 1979), 2.90)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 2.30 & Year <= 1978 & Year >= 1976), 2.30)) %>%
  mutate(Low.Value = replace(Low.Value, (Low.Value < 2.20 & Year <= 1978 & Year >= 1976), 2.20)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 1.60 & Year <= 1975 & Year >= 1972), 1.60)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 1.60 & Year <= 1971 & Year >= 1970), 1.60)) %>%
  mutate(Low.Value = replace(Low.Value, (Low.Value < 1.30 & Year <= 1971 & Year >= 1970), 1.30)) %>%
  mutate(High.Value = replace(High.Value, (High.Value < 1.60 & Year <= 1969 & Year >= 1968), 1.60)) %>%
  mutate(Low.Value = replace(Low.Value, (Low.Value < 1.15 & Year <= 1969 & Year >= 1968), 1.15))

change_equivalent2018 <- function(row, data) {
  if (row$High.2018 == 0) {
    year <- row$Year
    federal_equi <- data %>%
      filter(State == "Federal (FLSA)") %>%
      filter(Year == year) %>%
      select(High.2018)
    row$High.2018 = federal_equi
    row
  } else {
    row
  }
}

for (i in 1:nrow(changed)) {
  changed[i, ] = change_equivalent2018(changed[i, ], changed)
}

write.csv(changed, 'data/prepped_minimum_wage.csv', row.names = F)