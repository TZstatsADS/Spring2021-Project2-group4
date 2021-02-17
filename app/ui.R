### SHINY UI ###

ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Visualizing WEATHER Covid Correlates with Temperature</a>'), id="nav",
               windowTitle = "COVID-19 tracker",
               
               tabPanel("COVID-19 mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 150, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h3(textOutput("reactive_case_count"), align = "right"),
                                          h4(textOutput("reactive_death_count"), align = "right"),
                                          h6(textOutput("clean_date_reactive"), align = "right"),
                                          h6(textOutput("reactive_country_count"), align = "right"),
                                          plotOutput("epi_northern_curve", height="130px", width="100%"),
                                          plotOutput("epi_southern_curve", height="130px", width="100%"),
                                          
                                          sliderTextInput("plot_date",
                                                          label = h5("Select mapping date"),
                                                          choices = format(unique(cv_cases$date), "%d %b %y"),
                                                          selected = format(current_date, "%d %b %y"),
                                                          grid = FALSE,
                                                          animate=animationOptions(interval = 1500, loop = FALSE))
                                          
                            )
                        )
               ),
               
               tabPanel("Region plots",
                        sidebarLayout(
                            sidebarPanel(
                                
                                span(tags$i(h6("Adjust the slider to select a specific date range to display data over.")), style="color:#045a8d"),
                                span(tags$i(h6("Toggle between different measures/metrics of COVID-tracking.")), style="color:#045a8d"),
                                
                                
                                pickerInput("outcome_select", "Outcome:",   
                                            choices = c("Cases", "Deaths"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                sliderInput("date_range",
                                            "Date range:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=c(as.Date(cv_min_date), as.Date(current_date)),
                                            timeFormat="%d %b"),
                                
                                "Select outcome, from drop-down menu to update plots"
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                  tabPanel("New", plotlyOutput("northern_plot_new", width = "600px", height = "300px")),
                                  tabPanel("Cumulative", plotlyOutput("northern_plot_cumulative", width = "600px", height = "300px"))
                                ), 
                                
                                tabsetPanel(
                                  tabPanel("New", plotlyOutput("southern_plot_new", width = "600px", height = "300px")),
                                  tabPanel("Cumulative", plotlyOutput("southern_plot_cumulative", width = "600px", height = "300px"))
                                )
                            )
                        )
               ),
               
               tabPanel("Multivar. Time Series Prediction ",
                        fluidRow(selectInput("countryId3", label = "Choose dataset", choices = c("USA", "China", "Thailand","France", "Morocco", "Australia", "Mexico", "Taiwan", "Greece", "Canada"),
                                                      selected = NULL, multiple = FALSE)),
                                        plotOutput("plot1", height=500)
               ),
               
               tabPanel("Univar. Time Series Visualizations with ARIMA",
                        fluidRow(selectInput("countryId1", label = "Choose dataset", choices = c("USA", "China", "Thailand","France", "Morocco", "Australia", "Mexico", "Taiwan", "Greece", "Canada"),
                                                      selected = NULL, multiple = FALSE)),
                        fluidRow(column(width=10,
                                        numericInput("anum1", "ARIMA input 1:", 1),
                                        numericInput("anum2", "ARIMA input 2:", 2),
                                        numericInput("anum3", "ARIMA input 3:", 3),
                                        plotOutput("plot2", height=500, click="plot2_click"))
                        )),
               
               tabPanel("Multivar. Time Series Visualization ",
                        fluidRow(selectInput("countryId2", label = "Choose dataset", choices = c("USA", "China", "Thailand","France", "Morocco", "Australia", "Mexico", "Taiwan", "Greece", "Canada"),
                                                      selected = NULL, multiple = FALSE)),
                        plotOutput("multiplot", height=500)),
               
               tabPanel("NYC Time Series Prediction",
                        numericInput("predperiod", "How many days ahead to predict:", 5),
                        plotOutput("nycplot", height=500))
               )
    )
