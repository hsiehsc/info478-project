#state_list<-gsub(" ", ".", state.name, fixed=TRUE)
state_list <- state.name
homicide_raw <- read.csv("data/homicide_rates.csv", stringsAsFactors = F,
                         check.names = F)
data <- read.csv("data/prepped_minimum_wage.csv", stringsAsFactors = F)