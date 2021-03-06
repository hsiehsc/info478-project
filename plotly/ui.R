library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)
library(ggthemes)
library(reshape2)
library(tidyverse)

plotly_wage <- tabPanel(
    "Minimum Wage Map",
    titlePanel("Minimum Wage Choropleth by State and Year"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("year_slider", label = h3("Select Year"), min = 1968, 
                        max = 2017, value = 1993, sep = "", animate = TRUE)
        ),
        mainPanel(
            plotlyOutput("plotlymap")
        )    
))

homicide_graph <- tabPanel(
    "Homicide",
    titlePanel("Homicide Rates Graphed by State and Year"),
    sidebarLayout(
        selectizeInput("state_text_name", "Select State",
                    state_list
        ),
    mainPanel(
        plotlyOutput("homicide_graph")
    )
))

homicide_wage <- tabPanel(
  "Homicide and Wage",
  titlePanel("Homicide Rates and Minimum Wage Graphed Together"),
  sidebarLayout(
    selectizeInput("state_hom_wage", "Select State",
                   state_list
    ),
    mainPanel(
      plotlyOutput("p_hom_wage")
    )
  ))

shinyUI(navbarPage(
    # includeCSS("style.css"),
    theme = shinytheme("spacelab"),
    "An Analysis of Minimum Wage",
    plotly_wage,
    homicide_graph,
    homicide_wage
))
