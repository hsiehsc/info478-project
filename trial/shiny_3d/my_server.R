library("shiny")
library(plotly)
library(tidyverse)
library(dplyr)
library(shiny)
library(ggplot2)
library(snakecase)
library(tidyr)

source("maker.R")

my_server <- function(input, output) {
  set.seed(100)
  output$statee <- renderPlotly({
    return(maker(input$target))
  })
}

shinyUI(my_server)
