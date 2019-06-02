# jerry edu attainment

library(dplyr)
library(plotly)
library(RColorBrewer)

#- Read tables
edu_2009 <- read.csv("data/edu/ACS_09_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2010 <- read.csv("data/edu/ACS_10_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2011 <- read.csv("data/edu/ACS_11_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2012 <- read.csv("data/edu/ACS_12_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2013 <- read.csv("data/edu/ACS_13_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2014 <- read.csv("data/edu/ACS_14_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2015 <- read.csv("data/edu/ACS_15_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2016 <- read.csv("data/edu/ACS_16_5YR_S1501_with_ann.csv", stringsAsFactors = F)
edu_2017 <- read.csv("data/edu/ACS_17_5YR_S1501_with_ann.csv", stringsAsFactors = F)
states <- read.csv("data/edu/states.csv", stringsAsFactors = F)

#- Organize data into 1 table

# Function changes colnames into something understandable
change_colnames <- function(df) {
  colnames(df) <- c("geo_id1", "geo_id2", "state", "pop", "less_than_9th",
                    "9th_to_12th", "hs_grad", "some_col", "associates", "bachelors",
                    "graduate", "at_least_hs_grad", "at_least_bachelors",
                    "poverty_less_than_hs_grad", "poverty_hs_grad", 
                    "poverty_some_college_assoc", "poverty_at_least_bachelors",
                    "a", "b", "c", "d", "e", "f")
  
  return(df)
}

cols_as_numeric <- function(df) {
  return (mutate(df, less_than_9th = as.numeric(less_than_9th),
         `9th_to_12th` = as.numeric(`9th_to_12th`),
         hs_grad = as.numeric(hs_grad),
         some_col = as.numeric(some_col),
         associates = as.numeric(associates),
         bachelors = as.numeric(bachelors),
         graduate = as.numeric(graduate),
         at_least_hs_grad = as.numeric(at_least_hs_grad),
         at_least_bachelors = as.numeric(at_least_bachelors),
         poverty_less_than_hs_grad = as.numeric(poverty_less_than_hs_grad),
         poverty_hs_grad = as.numeric(poverty_hs_grad),
         poverty_some_college_assoc = as.numeric(poverty_some_college_assoc),
         poverty_at_least_bachelors = as.numeric(poverty_at_least_bachelors)))
}

# Function removes unneeded columns
select_cols <- function(df) {
  return(select(df, -pop, -a, -b, -c, -d, -e, -f))
}

# Change col names and remove useless columns
edu_2009 <- change_colnames(edu_2009) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2010 <- change_colnames(edu_2010) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2011 <- change_colnames(edu_2011) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2012 <- change_colnames(edu_2012) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2013 <- change_colnames(edu_2013) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2014 <- change_colnames(edu_2014) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2015 <- change_colnames(edu_2015) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2016 <- change_colnames(edu_2016) %>%
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()
edu_2017 <- change_colnames(edu_2017) %>% 
  select_cols() %>% 
  inner_join(states, by=c('state' = 'State')) %>%
  cols_as_numeric()

# Add col for year
edu_2009$year <- 2009
edu_2010$year <- 2010
edu_2011$year <- 2011
edu_2012$year <- 2012
edu_2013$year <- 2013
edu_2014$year <- 2014
edu_2015$year <- 2015
edu_2016$year <- 2016
edu_2017$year <- 2017

# Yeet
edu_df <- do.call("rbind", list(edu_2009, edu_2010, edu_2011, edu_2012, edu_2013,
                                   edu_2014, edu_2015, edu_2016, edu_2017))

# TO DO: 
#- choropleth of percentage > high school and percentage > bachelors
#- bar plot of percentages
#- percentage high school and percentage bachelors vs obesity

# Choropleth maps:

select_choro_yr <- filter(edu_df, year == 2009)

# choro bachelor complete
bachelor_choropleth <-
  plot_ly(
    select_choro_yr,
    type = "choropleth",
    z = ~ at_least_bachelors, # ui
    zmin = 15,
    zmax = 50,
    hoverinfo = "text",
    text = paste0(
      select_choro_yr$state, ": ", # ui
      select_choro_yr$at_least_bachelors, "%"
    ), # ui
    locations = ~ Abbreviation,
    locationmode = "USA-states",
    color = ~ at_least_bachelors,
    colors = brewer.pal(9, "YlGnBu"),
    colorbar = list(title = "Percentage")
  ) %>%
  layout(
    title = "Percentage of Residents with At Least A Bachelor's Degree", # ui
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = F
    )
  )

bachelor_choropleth

# choro of % high school complete
hs_choropleth <- 
  plot_ly(
    select_choro_yr,
    type = "choropleth",
    z = ~ at_least_hs_grad, # ui
    zmin = 75,
    zmax = 100,
    hoverinfo = "text",
    text = paste0(
      select_choro_yr$state, ": ", # ui
      select_choro_yr$at_least_hs_grad, "%"
    ), # ui
    locations = ~ Abbreviation,
    locationmode = "USA-states",
    color = ~ at_least_hs_grad,
    colors = brewer.pal(9, "YlGnBu"),
    colorbar = list(title = "Percentage")
  ) %>%
  layout(
    title = "Percentage of Residents with At Least A High School Diploma", # ui
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = F
    )
  )

hs_choropleth

# Horizontal Bar Plot of state and percentages of education attainment
selected_state_yr <- filter(edu_df, state == "Alabama", year == 2009)

state_bar_plot <- 
  plot_ly(selected_state_yr,
          y = ~state,
          x = ~less_than_9th,
          type = "bar",
          name = "Less than 9th grade",
          orientation = "h",
          hovertext = paste0("Less than 9th grade: ", selected_state_yr$less_than_9th, "%"),
          hoverinfo = "text",
          marker = list(color = "#ffffcc",
                        line = list(color = "#000000",
                                    width = 1))) %>%
  add_trace(x = ~`9th_to_12th`,
            name = "9th to 12th grade",
            hovertext = paste0("9th to 12th grade: ", selected_state_yr$`9th_to_12th`, "%"),
            marker = list(color = "#c7e9b4")) %>%
  add_trace(x = ~hs_grad,
            name = "High School Graduate",
            hovertext = paste0("High School Graduate: ", selected_state_yr$hs_grad, "%"),
            marker = list(color = "#7fcdbb")) %>%
  add_trace(x = ~some_col,
            name = "Some college",
            hovertext = paste0("Some college: ", selected_state_yr$some_col, "%"),
            marker = list(color = "#41b6c4")) %>%
  add_trace(x = ~associates,
            name = "Associate's Degree",
            hovertext = paste0("Associate's Degree: ", selected_state_yr$associates, "%"),
            marker = list(color = "#1d91c0")) %>%
  add_trace(x = ~bachelors,
            name = "Bachelor's Degree",
            hovertext = paste0("Bachelor's Degree: ", selected_state_yr$bachelors, "%"),
            marker = list(color = "#225ea8")) %>%
  add_trace(x = ~graduate,
            name = "Graduate Degree",
            hovertext = paste0("Graduate Degree: ", selected_state_yr$graduate, "%"),
            marker = list(color = "#0c2c84")) %>%
  layout(barmode = 'stack',
         title = paste("Percentage of Education Attainment in",
                        selected_state_yr$state, "in", selected_state_yr$year),
         yaxis = list(title=""), 
         xaxis = list(title="Percentage"),
         margin = list(pad = 10))

state_bar_plot  

# Join obesity data w/ education data for comparison
obese_2009 <- read.csv("trial/result_2010_2017/2009overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2010 <- read.csv("trial/result_2010_2017/2010overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2011 <- read.csv("trial/result_2010_2017/2011overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2012 <- read.csv("trial/result_2010_2017/2012overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2013 <- read.csv("trial/result_2010_2017/2013overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2014 <- read.csv("trial/result_2010_2017/2014overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2015 <- read.csv("trial/result_2010_2017/2015overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2016 <- read.csv("trial/result_2010_2017/2016overweightandobesity.csv",
                       stringsAsFactors = F)
obese_2017 <- read.csv("trial/result_2010_2017/2017overweightandobesity.csv",
                       stringsAsFactors = F)

colnames(obese_2009)[3] <- "x.rfbmi5"
colnames(obese_2010)[3] <- "x.rfbmi5"

obese_df <- do.call("rbind", list(obese_2009, obese_2010, obese_2011, obese_2012, 
                                  obese_2013, obese_2014, obese_2015, 
                                  obese_2016, obese_2017)) %>% 
  select(-X) %>%
  mutate(`x.state` = as.character(`x.state`))

combined_df <- left_join(edu_df, obese_df, 
                          by = c("geo_id2" = "x.state", "year" = "Year"))
