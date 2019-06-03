library(shiny)
library(leaflet)
library(shinythemes)
library(ggvis)
library(leaflet)
library(shinyWidgets)

minimum_wage <- read.csv("prepped_minimum_wage.csv")

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
            choices = list(
              unique(minimum_wage$State)
            ),
            selected = "Alabama"
          )
        ),
        mainPanel(plotlyOutput("statee"))
      )
    )
  )
)
shinyUI(my_ui)
