### SHINY UI ###

ui <- bootstrapPage(
    tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 tracker</a>'), id="nav",
               windowTitle = "COVID-19 tracker",
               
               tabPanel("COVID-19 mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
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
                                                          animate=animationOptions(interval = 3000, loop = FALSE))
                                          
                            )
                        )
               ),
               
               tabPanel("Region plots",
                        sidebarLayout(
                            sidebarPanel(
                                
                                span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),
                                
                                
                                pickerInput("outcome_select", "Outcome:",   
                                            choices = c("Cases", "Deaths"), 
                                            selected = c("Cases"),
                                            multiple = FALSE),
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b"),
                                
                                "Select outcome, from drop-down menu to update plots"
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("northern_plot_cumulative", width = "600px", height = "300px")),
                                    tabPanel("New", plotlyOutput("northern_plot_new", width = "600px", height = "300px"))
                                ), 
                                
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("southern_plot_cumulative", width = "600px", height = "300px")),
                                    tabPanel("New", plotlyOutput("southern_plot_new", width = "600px", height = "300px"))
                                )
                            )
                        )
               ),
               
               tabPanel("Time Series",
                        sidebarLayout(
                          sidebarPanel(

                            span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                            span(tags$i(h6("Occasional anomalies (e.g. spikes in daily case counts) are generally caused by changes in case definitions.")), style="color:#045a8d"),


                            pickerInput("country_select", "Country:",
                                        choices = as.character(unique(cv_cases$country)),
                                        options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                        selected = as.character(cv_cases$country)[1],
                                        multiple = FALSE),

                            "Select outcome, from drop-down menu to update plots"
                          )

                          # mainPanel(
               #              tabsetPanel(
               #                tabPanel("Cumulative", plotlyOutput("northern_plot_cumulative", width = "600px", height = "300px")),
               #                tabPanel("New", plotlyOutput("northern_plot_new", width = "600px", height = "300px"))
               #              ),
               # 
               #              tabsetPanel(
               #                tabPanel("Cumulative", plotlyOutput("southern_plot_cumulative", width = "600px", height = "300px")),
               #                tabPanel("New", plotlyOutput("southern_plot_new", width = "600px", height = "300px"))
               #              )
               #            )
                        )

               ),
               
               tabPanel("About this site",
                        tags$div(
                          tags$h4("Last update"), 
                          h6(paste0(update)),
                          "This site is updated once daily. There are several other excellent COVID mapping tools available, including those run by", 
                          tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "the WHO,"),
                          tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University,"),"and",
                          tags$a(href="https://ourworldindata.org/coronavirus-data-explorer?zoomToSelection=true&time=2020-03-01..latest&country=IND~USA~GBR~CAN~DEU~FRA&region=World&casesMetric=true&interval=smoothed&perCapita=true&smoothing=7&pickerMetric=total_cases&pickerSort=desc", "Our World in Data."),
                          "Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
                          
                          tags$br(),tags$br(),tags$h4("Background"), 
                          "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                          These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                          The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                          This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                          tags$br(),tags$br(),tags$h4("Background")
                        )
                      )
               )
    )