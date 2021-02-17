## preprocess
tc<- read.csv("tempcovid.csv")
tc1 <- tc %>% dplyr::select(date, country, cases, AVG.TEMP.)

# generate dataframes for countries 
# in global.R 
Usa <- tc1[tc1$country=="USA",]
China <- tc1[tc1$country=="Mainland China",]
Thailand <- tc1[tc1$country=="Thailand",]
France <- tc1[tc1$country=="France",]
Morocco <- tc1[tc1$country=="Morocco",]
Australia <- tc1[tc1$country=="USA",]



uitabs4 <- shinyUI( navbarPage("Weather and Covid", id="nav", theme="bootstrap.css",
                               tabPanel("Multivar. Time Series Prediction ",
                                        fluidRow(
                                          selectInput("countryId3", label = "Choose dataset", choices = c("Usa", "China", "Thailand","France", "Morocco", "Australia"),
                                                      selected = NULL, multiple = FALSE)
                                        ),
                                        plotOutput("plot1", height=500)
                               ),
                               
                               tabPanel("Univar. Time Series Visualizations with ARIMA",
                                        fluidRow(
                                          selectInput("countryId1", label = "Choose dataset", choices = c("Usa", "China", "Thailand","France", "Morocco", "Australia"),
                                                      selected = NULL, multiple = FALSE)
                                        ),
                                        fluidRow(column(width=10,
                                                        numericInput("anum1", "ARIMA input 1:", 1),
                                                        numericInput("anum2", "ARIMA input 2:", 1),
                                                        numericInput("anum3", "ARIMA input 3:", 1),
                                                        plotOutput("plot2", height=500, click="plot2_click"))
                                        )),
                               tabPanel("Multivar. Time Series Visualization ",
                                        fluidRow(
                                          selectInput("countryId2", label = "Choose dataset", choices = c("Usa", "China", "Thailand","France", "Morocco", "Australia"),
                                                      selected = NULL, multiple = FALSE)
                                        ),
                                        plotOutput("multiplot", height=500)),
                               tabPanel("NYC Time Series Prediction",
                                        numericInput("predperiod", "How many days ahead to predict:", 5),
                                        plotOutput("nycplot", height=500))
)
)





servertabs3 <- function(input, output) {
  
  #library(lubridate)
  
  
  output$val1 <- renderText({input$num1})
  
  # numeric input for arima 
  arima1 <- reactive({as.numeric(input$anum1)})
  arima2 <- reactive({as.numeric(input$anum2)})
  arima3 <- reactive({as.numeric(input$anum3)})
  
  
  startdate <- reactive({
    startdate <- as.POSIXct(paste(as.character(input$DateRange[1]),
                                  format = "%Y-%m-%d"))
  })
  
  enddate <- reactive({
    enddate <- as.POSIXct(paste(as.character(input$DateRange[2]),
                                format = "%Y-%m-%d"))
  })
  
  alabama$day <- as.Date(alabama$date)
  
  alabama1 <- reactive({
      alabama1 <- alabama %>% filter(day >= startdate(), day <= enddate())})
                                      
  
  #strdate <- reactive({get(input$startingdatechar)})
  
  #startdate <- as.Date(strdate, format="%m/%d/%Y")
  
  base <- reactive({get(input$dataId)}) 
  
  base_country1 <- reactive({get(input$countryId1)}) 
  
  base_country2 <- reactive({get(input$countryId2)})
  
  base_country3 <- reactive({get(input$countryId3)})
  
  output$dateText  <- renderText({
    paste("input$date is", as.character(input$daterange))
  })
  

  
  
  output$plot1 <- renderPlot({
    mydata1 <- base_country3()
    country_ts = as.ts(mydata1)
    country_var<- VAR(country_ts, p=1, type="const")
    country_forecast <- predict(country_var, n.ahead = 2, ci=0.95)
    plot(country_forecast)
  })
  
  
  output$click_info <- renderPrint({
    nearPoints(datas, input$plot1_click, addDist=T)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(datas, input$plot1_brush)
  })
  
  # try to plot brushed points
  output$plotbrush <- renderPlot({
    brushind <- input$plotbrush_brush
    ggplot(mtcars[brushind,],  aes(wt, mpg))+geom_point()
  })
  
  # also output the mean of the points (x,y) ?
  
  # add the time series analysis
  
  #ndays <- reactive({get(input$daysRange)})
  #ndays <- reactive({input$daysRange})
  
  txsubset = tx
  
  china2 <- china %>% dplyr::select(cases)
  
  ts_china = as.ts(china2)
  amodel_china = Arima(ts_china, order=c(1,2,3))
  
  # Arima model library(forecasts)
  amodel <- Arima(tx, order = c(1,2,3))
  amodel1 <- reactive({Arima(tx, order= c(arima1(),2,3))})
  
  # output time series plot
  output$plot2 <- renderPlot({
    countrybase_arima <- base_country1()
    arima_cases = countrybase_arima %>% dplyr::select(cases)
    uni_country_ts <- as.ts(arima_cases)
    # uni_country_ts <- country_ts %>% dplyr::select(cases)
    amodel <- Arima(uni_country_ts, order = c(1,2,3))
    
    
    # arima forecast
    plot(forecast(amodel_china, 5))
    
    #plot(forecast(amodel, 5))
    #plot(amodel1)
    # test time subsets 
    #corona1 <- reactive({alabama %>% dplyr::filter(between(day,  startdate,  as.Date("2020-03-30"))) })
    
    #ggplot()+ geom_line(data = alabama1, aes(day,cases), color="blue")
    
  })
  
  # multivariate ts library(vars)
  x1 <- diffinv(rnorm(500))
  x3 <- diffinv(rnorm(500))
  x2 <- diffinv(rnorm(500))
  x4 <- cbind(x1, x2, x3)
  xts <- as.ts(x4)
  Var.x <- VAR(xts, p=2, type="const")
  fcst.x <- forecast(Var.x)
  
  # 
  country_ts <- as.ts(china_ts)
  cvar<- VAR(country_ts, p=1)
  cforecast <- predict(cvar, n.ahead=3, ci=0.95, ylim= c(0,300))
  
  #fanchart(cforecast, names = "cases", main="cases forecast")
  
  output$multiplot <- renderPlot({
    # just plot the time series itself 
    data_multi <- base_country2()
    #plot(cforecast)
    # data =china
    data_multi$day = as.Date(data_multi$date)
    ggplot()+ geom_line(data = data_multi, aes(day,cases), color="blue")+xlab("Time")+ylab("Count")+geom_line(data= data_multi, aes(day, AVG.TEMP.), color="green")+scale_colour_manual(name = 'the colour', 
                            values =c('blue'='blue','green'='green'), labels = c('num. cases','avg. temp.'))
  })
  
  output$nycplot <- renderPlot({
    fanchart(hforecast, names = "hcount", main="hosp forecast")
  })
}
