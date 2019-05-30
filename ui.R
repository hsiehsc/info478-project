library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Minimum Wage by State by Year"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year_slider", label = h3("Select Year"), min = 1968, 
                        max = 2017, value = 1993)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("leafletmap")
        )
    )
))
