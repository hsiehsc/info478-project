library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)
library(leaflet)
library(shinyWidgets)
library(plotly)

minimum_wage <- read.csv("prepped_minimum_wage.csv")
code <- unique(minimum_wage$State)
names(code) <- unique(minimum_wage$State)


my_ui <- fluidPage(
  navbarPage(
    "Group Project",
    tabPanel(
      "Obesity, Overweight and Minimum Wage",
      titlePanel(h1("Obesity, Overweight and Minimum Wage")),
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            inputId = "target",
            label = h3("Select a State"),
            choices = unique(minimum_wage$State),
            selected = unique(minimum_wage$State)
          )
        ),
        mainPanel(plotlyOutput("statee"))
      )
    )
  )
)
shinyUI(my_ui)
