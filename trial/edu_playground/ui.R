library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)

edu_tab <-
  tabPanel(
    "Education Attainment",
    titlePanel(
      "Educational Attainment by State and its Relation to Obesity"
    ),
    tags$hr(),
    tags$p("The data for this segment was collected using the FactFinder table
           creation tool from the U.S. Census Bureau website in May 2019."),
    tags$hr(),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "shiny_yr", 
          label = h3("Select Year"), 
          min = 2009, 
          max = 2017, 
          value = 2009, 
          sep = "", 
          animate = TRUE
        )
      ),
      mainPanel(
        plotlyOutput("edu_choropleth"),
        textOutput("selection"),
        plotlyOutput("state_bar"),
        plotlyOutput("state_line_edu")
      )
    ),
    tags$hr(),
    tags$hr(),
    titlePanel(
      "Educational Attainment in Relation to Obesity Rate"
    ),
    tags$hr(),
    tags$p("This data was gathered from the U.S. Census Bureau like above and joined
           with data from the CDC's BRFSS survey datasets."),
    tags$p("An intersting observation is that even though the obesity rate is increasing
           , the gap in education rate is closing at a faster rate, leading
           to a less correlated result as the years go by."),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "shiny_yr2", 
          label = h3("Select Year"), 
          min = 2009, 
          max = 2017, 
          value = 2009, 
          sep = "", 
          animate = TRUE
        )
      ),
      mainPanel(
        plotlyOutput("obese_edu")
      )
    )
  )

shinyUI(navbarPage(
  theme = shinytheme("spacelab"),
  title = "test",
  edu_tab
))