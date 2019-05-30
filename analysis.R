library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))

state.abb[grep("New York", state.name)]
setNames(state.abb, state.name)[c("New York", "Idaho")]
x <- data$State

data$State <- state.abb[match(x,state.name)]


data_2017 <- data %>% 
  filter(Year == "2017")

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(data_2017, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Low.Value, locations = ~State,
    color = ~Low.Value, colors = 'Greens'
  ) %>%
  colorbar(title = "Minimum Wage") %>%
  layout(
    title = 'Minimum Wage by State<br>(Hover for breakdown)',
    geo = g
  )

