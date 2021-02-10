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
                                          plotOutput("epi_curve", height="130px", width="100%"),
                                          plotOutput("cumulative_plot", height="130px", width="100%"),
                                          
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
                                
                                pickerInput("level_select", "Level:",   
                                            choices = c("Global", "Continent", "Country", "US state"), 
                                            selected = c("Country"),
                                            multiple = FALSE),
                                
                                pickerInput("region_select", "Country/Region:",   
                                            choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country)[1:10],
                                            multiple = TRUE), 
                                
                                pickerInput("outcome_select", "Outcome:",   
                                            choices = c("Deaths per million", "Cases per million", "Cases (total)", "Deaths (total)"), 
                                            selected = c("Deaths per million"),
                                            multiple = FALSE),
                                
                                pickerInput("start_date", "Plotting start date:",   
                                            choices = c("Date", "Week of 100th confirmed case", "Week of 10th death"), 
                                            options = list(`actions-box` = TRUE),
                                            selected = "Date",
                                            multiple = FALSE), 
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b"),
                                
                                "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 1000 confirmed cases are included."
                            ),
                            
                            mainPanel(
                                tabsetPanel(
                                    tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                                    tabPanel("New", plotlyOutput("country_plot")),
                                    tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                                )
                            )
                        )
               ),
               
               tabPanel("Data",
                        numericInput("maxrows", "Rows to show", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                           "Johns Hopkins Center for Systems Science and Engineering.")
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
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                            tags$b("US state-level case data: "), tags$a(href="https://github.com/nytimes/covid-19-data", "New York Times github page,"),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                        )
               )
               
    )          
)