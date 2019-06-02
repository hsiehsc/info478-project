library(foreign)
library(survey)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(knitr)


LLC2017_data <- LLC2017_data %>%
                  filter((x.state <= 56))
LLC2017_data <- LLC2017_data %>%
                  select(x.state, x.rfbmi5, x.llcpwt, x.psu)
LLC2017_data <- LLC2017_data %>%
                  mutate(
                    x.rfbmi5 = replace(x.rfbmi5, x.rfbmi5 == 9, NA),
                    x.rfbmi5 = replace(x.rfbmi5, x.rfbmi5 == 1, 0),
                    x.rfbmi5 = replace(x.rfbmi5, x.rfbmi5 == 2, 1)
                  )
Obesity_2017_data <- svydesign(
                            weights = ~x.llcpwt,
                            data = LLC2017_data,
                            id = ~x.psu,
                            nest = TRUE
                          )

svyby(~x.rfbmi5, ~x.state, Obesity_2017_data, svymean, na.rm = T) -> result_2017
svymean(~x.rfbmi5, Obesity_2017_data, na.rm = T) -> mean_2017
write.csv(result_2017, "2017overweightandobesity.csv")
result_2017 <- result_2017 %>%
                mutate(Year = "2017")

