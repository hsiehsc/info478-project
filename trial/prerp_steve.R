library(foreign)
library(survey)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(knitr)

data2001 <- sasxport.get("CDBRFS01.XPT")

LLC2001_data <- data2001 %>%
  filter((x.state <= 56))

LLC2001_data <- LLC2001_data %>%
  select(x.state, x.rfbmi2, x.finalwt, x.psu)

LLC2001_data <- LLC2001_data %>%
  mutate(
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 9, NA),
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 1, 0),
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 2, 1)
  )

Obesity_2001_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2001_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi2, ~x.state, Obesity_2001_data, svymean, na.rm = T) -> result_2001

svymean(~x.rfbmi2, Obesity_2001_data, na.rm = T) -> mean_2001

result_2001 <- result_2001 %>%
  mutate(Year = "2001")


write.csv(result_2001, "result_2001_2009/2001overweightandobesity.csv")

data2002 <- sasxport.get("cdbrfs02.xpt")

LLC2002_data <- data2002 %>%
  filter((x.state <= 56))

LLC2002_data <- LLC2002_data %>%
  select(x.state, x.rfbmi2, x.finalwt, x.psu)

LLC2002_data <- LLC2002_data %>%
  mutate(
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 9, NA),
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 1, 0),
    x.rfbmi2 = replace(x.rfbmi2, x.rfbmi2 == 2, 1)
  )

Obesity_2002_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2002_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi2, ~x.state, Obesity_2002_data, svymean, na.rm = T) -> result_2002

svymean(~x.rfbmi2, Obesity_2002_data, na.rm = T) -> mean_2002

result_2002 <- result_2002 %>%
  mutate(Year = "2002")

write.csv(result_2002, "result_2001_2009/2002overweightandobesity.csv")


data2003 <- sasxport.get("cdbrfs03.xpt")

LLC2003_data <- data2003 %>%
  filter((x.state <= 56))

LLC2003_data <- LLC2003_data %>%
  select(x.state, x.rfbmi3, x.finalwt, x.psu)

LLC2003_data <- LLC2003_data %>%
  mutate(
    x.rfbmi3 = replace(x.rfbmi3, x.rfbmi3 == 9, NA),
    x.rfbmi3 = replace(x.rfbmi3, x.rfbmi3 == 1, 0),
    x.rfbmi3 = replace(x.rfbmi3, x.rfbmi3 == 2, 1)
  )

Obesity_2003_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2003_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi3, ~x.state, Obesity_2003_data, svymean, na.rm = T) -> result_2003

svymean(~x.rfbmi3, Obesity_2003_data, na.rm = T) -> mean_2003

result_2003 <- result_2003 %>%
  mutate(Year = "2003")

write.csv(result_2003, "result_2001_2009/2003overweightandobesity.csv")


data2004 <- sasxport.get("cdbrfs04.xpt")

LLC2004_data <- data2004 %>%
  filter((x.state <= 56))

LLC2004_data <- LLC2004_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2004_data <- LLC2004_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2004_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2004_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2004_data, svymean, na.rm = T) -> result_2004

svymean(~x.rfbmi4, Obesity_2004_data, na.rm = T) -> mean_2004

result_2004 <- result_2004 %>%
  mutate(Year = "2004")

write.csv(result_2004, "result_2001_2009/2004overweightandobesity.csv")


data2005 <- sasxport.get("CDBRFS05.XPT")

LLC2005_data <- data2005 %>%
  filter((x.state <= 56))

LLC2005_data <- LLC2005_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2005_data <- LLC2005_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2005_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2005_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2005_data, svymean, na.rm = T) -> result_2005

svymean(~x.rfbmi4, Obesity_2005_data, na.rm = T) -> mean_2005

result_2005 <- result_2005 %>%
  mutate(Year = "2005")

write.csv(result_2005, "result_2001_2009/2005overweightandobesity.csv")


data2006 <- sasxport.get("CDBRFS06.XPT")

LLC2006_data <- data2006 %>%
  filter((x.state <= 56))

LLC2006_data <- LLC2006_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2006_data <- LLC2006_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2006_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2006_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2006_data, svymean, na.rm = T) -> result_2006

svymean(~x.rfbmi4, Obesity_2006_data, na.rm = T) -> mean_2006

result_2006 <- result_2006 %>%
  mutate(Year = "2006")

write.csv(result_2006, "result_2001_2009/2006overweightandobesity.csv")


data2007 <- sasxport.get("CDBRFS07.XPT")

LLC2007_data <- data2007 %>%
  filter((x.state <= 56))

LLC2007_data <- LLC2007_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2007_data <- LLC2007_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2007_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2007_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2007_data, svymean, na.rm = T) -> result_2007

svymean(~x.rfbmi4, Obesity_2007_data, na.rm = T) -> mean_2007

result_2007 <- result_2007 %>%
  mutate(Year = "2007")

write.csv(result_2007, "result_2001_2009/2007overweightandobesity.csv")


data2008 <- sasxport.get("CDBRFS08.XPT")

LLC2008_data <- data2008 %>%
  filter((x.state <= 56))

LLC2008_data <- LLC2008_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2008_data <- LLC2008_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2008_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2008_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2008_data, svymean, na.rm = T) -> result_2008

svymean(~x.rfbmi4, Obesity_2008_data, na.rm = T) -> mean_2008

result_2008 <- result_2008 %>%
  mutate(Year = "2008")

write.csv(result_2007, "result_2001_2009/2008overweightandobesity.csv")


data2009 <- sasxport.get("CDBRFS09.XPT")

LLC2009_data <- data2009 %>%
  filter((x.state <= 56))

LLC2009_data <- LLC2009_data %>%
  select(x.state, x.rfbmi4, x.finalwt, x.psu)

LLC2009_data <- LLC2009_data %>%
  mutate(
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 9, NA),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 1, 0),
    x.rfbmi4 = replace(x.rfbmi4, x.rfbmi4 == 2, 1)
  )

Obesity_2009_data <- svydesign(
  weights = ~x.finalwt,
  data = LLC2009_data,
  id = ~x.psu,
  nest = TRUE
)

svyby(~x.rfbmi4, ~x.state, Obesity_2009_data, svymean, na.rm = T) -> result_2009

svymean(~x.rfbmi4, Obesity_2009_data, na.rm = T) -> mean_2009

result_2009 <- result_2009 %>%
  mutate(Year = "2009")
write.csv(result_2007, "result_2001_2009/2009overweightandobesity.csv")