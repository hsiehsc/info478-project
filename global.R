# Reading in relevant libraries
library(dplyr)
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(ggthemes)
library(reshape2)
library(tidyverse)

# Reading in Variables
state_list <- state.name
homicide_raw <- read.csv("data/homicide_rates.csv",
  stringsAsFactors = F,
  check.names = F
)
data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)

# Prepping minimum wage data
minimum_wage <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)
minimum_wage <- minimum_wage %>%
  filter(Year >= 2001)
minimum_wage <- minimum_wage %>%
  filter(State != "District of Columbia")
minimum_wage <- minimum_wage %>%
  filter(State != "Federal (FLSA)" & State != "Guam" &
    State != "Puerto Rico" & State != "U.S. Virgin Islands")

code <- unique(minimum_wage$State)
names(code) <- unique(minimum_wage$State)

# Reading in obesity data
all_data <- read.csv("data/alldata.csv", stringsAsFactors = F)
all_data <- all_data %>%
  filter(x.state != 11)
all_data$X <- state.name
