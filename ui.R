introduction <- tabPanel(
  "Introduction",
  mainPanel(
    tags$h4(strong("Information about Minimum Wages",
      style = "color: #2C3E50;"
    )),
    tags$h6(em("June 5th, 2019")),
    tags$p("The minimum wage in the United States is set by both US and
           State laws and since July 24, 2009, the minimum wage has been
           $7.25 an hour. However, recently different states and cities have
           been experimenting with setting higher minimum wages.
           Financially, proponents of
           higher minimum wages believe it will raise worker productivity,
           reduce turnover, reduce absenteeism, and stimulate the economy.
           Meanwhile, those against raising minimum wages believe that
           raising minimum wages would force businesses to lay off
           employees, raise unemployment levels, increase poverty, and
           increase the price of consumer goods. However, minimum wage doens't
           just have financial effects, but spans into the realm of public
           health. There are many arguments to
           be had looking at which groups within the US are disproportionaly
           affected by a change in minimum wages, and the debate continues
           even to this day. This project aims to take a broad look at minimum
           wage and how it might be correlated to obesity rates, education,
           and homicide rates. However, as each topic is broad and full of
           multiple caveats, drawing a conclusion is difficult. Instead, this
           project hopes to open up a conversation regarding minimum wage
           and encourage conversation into what other topics might be worth
           looking into. "),
    tags$h4(strong("Information about the Datasets", style = "color: #2C3E50;")),
    tags$p(
      "The information was collected by",
      em("Lislejoem"), "and sourced by
          the US Department of Labor's website. The dataset was hosted on Kaggle
          and is targeted towards
          individuals who have an interest in seeing the effects of
          minimum wage. This data set can help answer questions on how minimum
      wage might affect public health."
    ),
    tags$a(
      href = "https://www.kaggle.com/lislejoem/us-minimum-wage-by-state-from-1968-to-2017",
      "You can find a link to the minimum wage data by clicking here."
    ),
    tags$br(),
    tags$br(),
    tags$p(
      "The educational attainment data table was built by Jerry Lin from the U.S
      Census Bureau FactFinder tool.  This data is used to find insights in education
      attainment levels and includes data on poverty levels in relation to education
      attainment also."
    ),
    tags$a(
      href = "https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_17_5YR_S1501&prodType=table",
      "The base table for education attainment can be found here."
    ),
    tags$h4(strong("Questions that can be asked", style = "color: #2C3E50;")),
    tags$p("The data set is 2,750 rows long and summarizes information by
            Year, State, Table Data, Footnote, High.Value (the higher of the
            minimum wage values provided), Low.Value (the lower of the minimum
            wage values provided), CPI.Average (Average Consumer Price Index
            for that year), High.2018 (2018 equivalent dollars for High.Value),
            and Low.2018 (2018 equivalent dollars for Low.Value).
            Some of the questions that we
            attempt to answer with this data set include:"),
    tags$ol(
      tags$li("Which state has the highest minimum wage?"),
      tags$li("How do obesity rates, homicide rates, and education rates
              link to minimum wage?"),
      tags$li("Is there a clear connection between more purchasing power
              and lower crime rates?"),
      tags$li("How much have education rates been increasing each year?"),
      tags$li("Which years had the highest/lowest rates of crime?")
    ),
    tags$h4(strong("Structure", style = "color: #2C3E50;")),
    tags$p("The first tab of this report shows has a
            choropleth map of the minimum wage in each state.
            Users can change the year
            input to see how minimum wages vary in each
            state from 1968 to 2017.
            The second page shows homicide rates graphed by year as well as a
            plot of
            homicide rates graphed by minimum wages.
            Users can select the state
            and see how the homicide rate for that state over time as well as
            how the minimum wage in 2018 dollars is correlated with homicide
            rates.
            The third plot is a 3D plot that
            that looks at obesity/overweight statistics and the minimum wage
            plotted against time from 2001 - 2017.
           Users can select which state(s) they want to analyze and see general
           trends of obesity rates and the minimum wage.
           The fourth plot analyzes the relationship between educational
           attainment and minimum wage. Finally, the fifth tab analyzes how
           obesity might be related to minimum wage."),
    tags$h4(strong("Creators", style = "color: #2C3E50;")),
    tags$ol(
      tags$li("Steven Hsieh"),
      tags$li("Jerry Lin"),
      tags$li("Steve Ma"),
      tags$li("Weixing Nie")
    )
  )
)

plotly_wage <- tabPanel(
  "Minimum Wage Map",
  titlePanel("Minimum Wage Choropleth by State and Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_slider",
        label = h3("Select Year"), min = 1968,
        max = 2017, value = 2017, sep = "", animate = TRUE
      )
    ),
    mainPanel(
      tags$h4(strong("Minimum Wage Choropleth "), align = "center"),
      plotlyOutput("plotlymap"),
      tags$p("This choropleth shows the minimum wage for each state in the
          United States from 1968-2017. Each state is graphed on this plot
          including Alaska and Hawaii. Keep in mind that some states do not
          have an official minimum wage or have state minimum wages
          less than federal minimum wage levels. If the federal wage is higher
          than the state wage, we have replaced the state wage with the federal
          level as that is the wage they would have been paid. Often states with
          lower documented minimum wages do not update their wage to reflect
          the federal level as the employees are being paid the federal level
          anyways, and updating the wage would be an inefficient use of
                 resources."),
      tags$p(
        strong("To use this map:"), "Look at the side panel on the
              top left side
              of the page. You can select the year of interest with the",
        strong("Select Year"), "slider. Once you select the year,
              the choropleth will display
              the information for the minimum wage of that year. Upon hovering
            over each state, detailed information will be shown including the
            Low.Value for that state, the High.Value adjusted for 2018 dollars,
            and the Low.Value adjusted for 2018 dollars. You can also
            press the play button located in the bottom right of the Select Year
            slider to have the plot automatically progress through each year."
      ),
      tags$h5(strong(em("Possible Questions You Could Ask About the Plot:",
        style = "color: #2C3E50;"
      ))),
      tags$ol(
        tags$li("Which state has the highest minimum wage in 2017?"),
        tags$li("When did states begin to enact their own minimum wages
                    different from federal levels?"),
        tags$li("How have specific minimum wage values changed over time
                    to match with inflation?")
      ),
      tags$h5(
        strong("Insights:", style = "color: #2a8e0e;")
      ),
      tags$p("Generally, most of the states have the same minimum wage as
                 set by the federal level from 1968 to about the late 1990s.
                 However, once the 2000s come, we see more segmentation in
                 states and their minimum wage. Interestingly, Alaska seems to
                 be a pioneer of setting higher minimum wages than the federal
                 average. Once the time period is near 2010, the west coast
                 take a lead in setting higher than average minimum wages, with
                 the east coast soon following. However, much of the deep South
                 and areas surrounding Texas often stick to the federal minimum
                 wage. Surprisingly, in 1985, the federal minimum wage was only
                 $3.35, which speaks to how inflation has changed this wage over
                 time.")
    )
  )
)

homicide_graph <- tabPanel(
  "Homicide",
  titlePanel("Homicide Rates Graphed by State and Year"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "state_text_name", "Select State",
        state_list
      )
    ),
    mainPanel(
      tags$h4(strong("Homicide Rate Plot"), align = "center"),
      div(class = "span6", plotlyOutput("homicide_graph")),
      tags$h4(strong("Homicide vs Minimum Wage Plot"),
        align = "center"
      ),
      div(class = "span6", plotlyOutput("p_hom_wage")),
      tags$p("These two plots demonstrate homicide rates over time per state as
    well as those same homicide rates over time plotted against the minimum wage
    adjusted for 2018 dollars of that state. For consistency, homicide rates
    have been plotted on the Y-Axis. In the first plot, the year has been
    plotted on the X-Axis. In the second plot, adjusted 2018 minimum wages have
    been plotted on the X-Axis."),
      tags$p(
        strong("To use this map:"), "Look at the side panel on the
              top left side
              of the page. You can select the state of interest with the",
        strong("Select State"), "drop-down menu. Once you select the state,
              the two plots will display
              the information for the homicide rates of that state. Both plots
      will automatically update for the requested state when selected."
      ),
      tags$h5(strong(em("Possible Questions You Could Ask About these Plots:",
        style = "color: #2C3E50;"
      ))),
      tags$ol(
        tags$li("Which year had the highest homicide rates for X State?"),
        tags$li("Is there a strong correlation between adjusted minimum wages
              and the homicide rate? How does this correlation change
              between states?"),
        tags$li("Why is the Homicide vs. Minimum Wage plot considerably messier
              than the Homicide Rate Plot? What might this suggest?")
      ),
      tags$h5(
        strong("Insights:", style = "color: #2a8e0e;")
      ),
      tags$p("Each state often paints a starkly different
      picture when it comes to
           how homicide rates have changed throughout the years. While states
           like South Carolina and Washington have similar trend lines where
           they sharply rise until about 1993, and then begin to decline, states
           like North Dakota have experienced a rapid explosion in homicide
           rates
           in the last few years of the plot. States like New York have done an
           excellent job bringing homicide rates but Vermont has not experienced
           the same level of success.
           Meanwhile, when looking at the Homicide vs. Minimum Wage plot, we can
           see that generally as minimum wage increases (as we go farther right
           on the plot), the homicide rate will decrease (go down on the plot).
           However, this correlation doesn't necessarily mean causation. Crime
           rates have on average been decreasing throughout the years, and
           many states have been raising their minimum wage. Despite this, each
           state paints a rather fascinating picture of their homicide and
           minimum wage, and instead of looking at the plots as a whole, it
           might be more useful to compare states to states.")
    )
  )
)

### Creating 3d Plot tabPanel
shiny_3d <- tabPanel(
  "Obesity and Minimum Wages",
  titlePanel(h1("Obesity/Overweight Percentages and Minimum Wage")),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "target",
        label = h3("Select a State"),
        # Choices to select states
        choices = append("None", append("All",
          unique(minimum_wage$State))),
        selected = "All"
      )
    ),
    mainPanel(
      tags$h4(strong("3D Plot"), align = "center"),
      plotlyOutput("statee"), tags$p("
        This 3D plot demonstrates overweight rates vs minimum wage over time per
        state from
        2001 to 2017. The data is from the Behavioral Risk Factor Surveillance
        System (BRFSS). From 2001 to 2017, the BRFSS has given surveys to
        measure the obesity problem in the US.
        For consistency, the year have been plotted on the X-Axis,
        minimum wage have
        been plotted on the Z-Axis, and states have been plotted on the Y-Axis.
        The overweight percentage
        is represented by colors. The brighter the dot means the higher
        percentage in overweight rates. Note: The ratio presented on the color
        bar is out of 1. For example, 0.7 means 70% of the population is
                                     either overweight or obese."),
      tags$p(
        strong("To use this map:"), "To use this map, look at the side panel on
        the left. In the", strong("Select a State"), "input box, users can
        select the state(s) that they want to display in the 3d plot. Users can
        then use their mouse to rotate around the 3D plot. The mouse wheel
        allows for the ability to zoom in or zoom out. When the
        mouse points at a particular dot, it will present the state name and
        other relevant information."
      ),
      tags$h5(strong(em("Possible Questions You Could Ask About these Plots:",
        style = "color: #2C3E50;"
      ))),
      tags$ol(
        tags$li("What is the overall tendency of obesity?"),
        tags$li("Is the difference in minimum wage leading to the difference of obesity?"),
        tags$li("Is there an obvious difference between Northern states and Southern states?")
      ),
      tags$h5(
        strong("Insights:", style = "color: #2a8e0e;")
      ),
      tags$p("In general,
             most of the states are facing an increasing overweight problem.
             Approximately, around sixty percent of the US citizens
             are facing this public health problem. Particularly, West Virgina,
             Oklahoma, Alabama and Arkansas have 70% of the population measured
             as overweight. This 3D plot also shows that there isn't a strong
             correlation between minimum wage and overweight percentages.
             Based on all 
             current information, even there isn't a strong relationship
             between minimum wage and
             overweight percentage, it is evident that the US is currently
             facing a serious obesity problem.")
    )
  )
)

### About Us tabPanel
about_us <- tabPanel(
  "Meet the Team",
  headerPanel(
    h4("About Us",
      style = "font-family: 'Arial';cursive;
         font-size: 25px; line-height: 1.0:
         color: #36454f;
         text-align: center;
         font-weight: 300;"
    )
  ),
  tags$h4("This project was created by a team of four students in INFO 478
            at the University of Washington. It was created by",
    strong("Steven Hsieh,
        Jerry Lin,
        Steve Ma,
        and Weixing Nie."),
    align = "center"
  ),
  sidebarPanel(
    tags$h4(strong("Steven Hsieh"), align = "middle"),
    tags$img(
      src = "http://tinyurl.com/y2m2n76j", height = "200 px",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    tags$p("Steven Hsieh is a Junior at the University of Washington
            studying Finance. With R, he wishes to make pretty
            graphs and cool visualizations that also show information in
            a meaningful way.")
  ),
  sidebarPanel(
    tags$h4(strong("Jerry Lin"), align = "center"),
    tags$img(
      src = "https://i.imgur.com/pymLtTS.png", height = "200 px",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    tags$p("Jerry Lin is a sophomore majoring in Informatics with an intended
           concentration in data science.  In the future, Jerry hopes to ideally
           work in the field of computational linguistics or be a code monkey.
           During his spare time, Jerry enjoys playing video games, studying
           linguistics, and reading books.")
  ),
  sidebarPanel(
    tags$h4(strong("Steve Ma"), align = "center"),
    tags$img(
      src = "https://i.imgur.com/6ydZDZn.png", height = "200 px",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    tags$p("Steve Ma is a UW Sophomore majoring in Applied & Computational
           Mathematical Science with a focus in Scientific Computing,
           Numerical Algorithms and Computer Science. In the future, Steve wants
           to work in the aspect of Applied Math, while simultaneously
           introducing younger students to the basics of coding and setting up
           a strong foundation in Math.")
  ),
  sidebarPanel(
    tags$h4(strong("Weixing Nie"), align = "center"),
    tags$img(
      src = "https://i.imgur.com/19gQVEF.jpg", height = "200 px",
      style = "display: block; margin-left: auto; margin-right: auto;"
    ),
    tags$p("Oliver Nie/Weixing Nie is currently an undergraduate student of UW.
           He is interested in data science, which is a reason why he wishes
           to study informatics. Weixing believes in the power of data science
           to correct contemporary mistakes of human beings. His dream job is to
           become a video game director, and he truly wants to develop a game
           that can be recognized as the candidate for Nobel Literature Prize.
           In his vision, the future of video games is a small scale of a
           reality simulation.")
  )
)

# Education Attainment
edu_tab <-
  tabPanel(
    "Education Attainment",
    titlePanel(
      "Educational Attainment by State and Relation to Minimum Wage"
    ),
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
        tags$h4(strong("Minimum Wage Choropleth "), align = "center"),
        plotlyOutput("edu_choropleth"),
        textOutput("selection"),
        plotlyOutput("state_bar"),
        tags$h4(strong("Minimum Wage and Education Attainment vs Year")),
        plotlyOutput("state_line_edu"),
        tags$hr(),
        tags$p("This choropleth map displays the percentage of the population over age 25
               from the state that completed at least high school between the years
               2009 and 2017, which we are using to represent the level of education attainment in a state.  
               Upon click, a more detailed breakdown of educational
               attainment by state and year as well as a line graph detailing the
               relationship between minimum wage levels and education attainment 
               over time will be generated.  The data for these plots was gathered
               from the U.S. Census Bureau as well as the U.S Department of Labor
               by proxy of kaggle.com in May 2019."),
        tags$p(
          strong("To use this map:"), "Look at the side panel on the
          top left side
          of the page. You can select the year of interest with the",
          strong("Select Year"), "slider. Once you select the year,
          the choropleth will display data for the percentage of state population above
          the age of 25 that completed at least high school.  Hover over a state to view
          exact percentages and click on a state to generate a more detailed proportional
          plot of educational attainment by state and a graph modeling the relationship
          between minimum wage and educational attainment over time (double-click to clear,
          hover for more information).  For the minimum 
          wage vs. educational attainment graph, a linear or positive trendline represents a roughly
          linear relationship between minimum wage and educational attainment,
          a positive trendline represents an inverse relationship.  You can also press
          the \"play\" button on the side panel to automatically go through all the years."
        ),
        tags$h5(strong(em("Possible Questions You Could Ask About the Plot:",
                          style = "color: #2C3E50;"
        ))),
        tags$ol(
          tags$li("What percent of Washingtonians completed less than 9th grade in 2017?"),
          tags$li("Which state/region has the highest educational attainment level?"),
          tags$li("How does the education attainment level change as the minimum wage
                  changes in Wyoming?")
          ),
        tags$h5(
          strong("Insights:", style = "color: #2a8e0e;")
        ),
        tags$p("Something interesting is that there seems to be a distinctive north/south
               divide in education attainment.  It seems that across the board,
               states in the south have education attainment levels around 6-10% lower
               than those in the north which is a interesting phenomenon.  
An interesting observation that can be made from the plot is that apart from the 
District of Columbia, the state with the percentage of denizens with graduate degrees
is Massachusetts at 16.1% which means that almost 1 in 6 people have a graduate degree.  Perhaps 
one could posit that this has something to do with the sheer number of
prestigious institutions of education in that state but that number is quite something.
Additionally,
               we see that it's hard to determine if there's any real relationship between
               minimum wage and education attainment.  For every state, education attainment
               increases regardless of what is happening to the minimum wage.")
      )
    )
  )

obese_tab <- 
  tabPanel(
    "Obesity Rates",
    titlePanel(
      "Obesity Rate by State and Relation to Minimum Wage"
    ),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "obese_yr", 
          label = h3("Select Year"), 
          min = 2009, 
          max = 2017, 
          value = 2009, 
          sep = "", 
          animate = TRUE
        )
      ),
      mainPanel(
        tags$h4(strong("Obesity/Overweight Ratio Choropleth "), align = "center"),
        plotlyOutput("obese_choro"),
        textOutput("selection2"),
        plotlyOutput("obese_line"),
        tags$hr(),
        tags$p("This choropleth map displays the ratio of the population from each state
               that were deemed overweight or obese from the years 2009 to 2017.  The graph
               generated on click is of this ratio versus the minimum wage over the same 
               years.  The data for these plots were sourced from CDC BRFSS surveys and
               from the U.S. Department of Labor by proxy of kaggle.com.  States in white
               are missing data."),
        tags$p(
          strong("To use this map:"), "Look at the side panel on the
          top left side
          of the page. You can select the year of interest with the",
          strong("Select Year"), "slider. Once you select the year,
          the choropleth will display data for the ratio of state population considered
          to be at least overweight.  Hover over a state to view
          exact percentages ratios and click on a state to generate a graph modeling the relationship
          between minimum wage and overweight ratio over time (double click to clear,
          you can also hover and view exact minimum wage and overweight ratio numbers).  For the minimum 
          wage vs. overweight rate graph, a linear or positive trendline represents a roughly
          relationship between minimum wage and overweight ratio such that as minimum wage increases,
          overweight ratio decreases, and
          a positive trendline represents the opposite."
        ),
        tags$h5(strong(em("Possible Questions You Could Ask About the Plot:",
                          style = "color: #2C3E50;"
        ))),
        tags$ol(
          tags$li("What ratio of Oregonians were overweight in 2017?"),
          tags$li("Which state/region has the highest rate of obesity/overweight-ness?"),
          tags$li("How does the overweight ratio in Kansas change as the minimum wage
                  changes?")
        ),
        tags$h5(
          strong("Insights:", style = "color: #2a8e0e;")
        ),
        tags$p("The Midwest and South have distinctively greater rates of obesity than
               the states west of the Rockies and New England.  It can be noted that in
               2017, the top 10 states are all in this subset and that 7 of them border
               each other (TX, OK, LO, MS, TN, KY, WV).  It can also be noted that
               in every state, the ratio of the population considered to be overweight
               has risen between 2009 and 2017 by around 2-3% on average.")
      )
    )
  )

shinyUI(navbarPage(
  # includeCSS("style.css"),
  theme = shinytheme("flatly"),
  "An Analysis of Minimum Wage",
  introduction,
  plotly_wage,
  homicide_graph,
  shiny_3d,
  edu_tab,
  obese_tab,
  about_us
))
