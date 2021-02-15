
uitabs <- shinyUI( navbarPage("Weather and Covid", id="nav", theme="bootstrap.css",
                              tabPanel("Interactive Map",
                                       fluidRow(
                                         numericInput("num1", "Input number:", 1),
                                         verbatimTextOutput("val1")
                                       )),
                              tabPanel("Time Series Visualizations",
                                       fluidRow(column(width=10,
                                                       sliderInput("Days-Range",
                                                                   label="Range of Days:",
                                                                   min=0,max=100, value = c(0,100)),
                                                       numericInput("anum1", "ARIMA input 1:", 1),
                                                       numericInput("anum2", "ARIMA input 2:", 1),
                                                       numericInput("anum3", "ARIMA input 3:", 1),
                                                       plotOutput("plot2", height=500, click="plot2_click")))),
                              tabPanel("Data Inspection",
                                       fluidRow(
                                         selectInput("dataId", label = "Choose dataset", choices = c("mtcars", "mtcars2"),
                                                     selected = NULL, multiple = FALSE)
                                       ),
                                       fluidRow(
                                         column(width = 4,
                                                plotOutput("plot1", height = 300,
                                                           # Equivalent to: click = clickOpts(id = "plot_click")
                                                           click = "plot1_click",
                                                           brush = brushOpts(
                                                             id = "plot1_brush"
                                                           )
                                                ),
                                                plotOutput("plotbrush", height=300, brush=brushOpts(id="plotbrush_brush"))
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                h4("Points near click"),
                                                verbatimTextOutput("click_info")
                                         ),
                                         column(width = 6,
                                                h4("Brushed points"),
                                                verbatimTextOutput("brush_info")
                                         )
                                       )
                                       )
                              )
)

servertabs <- function(input, output) {
  
  output$val1 <- renderText({input$num1})
  
  base <- reactive({get(input$dataId)}) 
  
  # need them to have the same column names (?)
  
  #output$plot1 <- renderPlot({
  #  ggplot(datas, aes(wt, mpg)) + geom_point()
  #}) 
  output$plot1 <- renderPlot({
    mydata <- base()
    ggplot(mydata, aes(wt, mpg))+ geom_point()
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
  
  # Arima model library(forecasts)
  amodel <- Arima(tx, order = c(1,2,3))
  
  # output time series plot
  output$plot2 <- renderPlot({
    # arima forecast
    plot(forecast(amodel, 5))
  })
  
}






uitabs2 <- shinyUI( navbarPage("Weather and Covid", id="nav", theme="bootstrap.css",
                              tabPanel("Interactive Map",
                                       fluidRow(
                                         numericInput("num1", "Input number:", 1),
                                         verbatimTextOutput("val1")
                                       )),
                              tabPanel("Time Series Visualizations",
                                       fluidRow(column(width=10,
                                                
                                                       dateRangeInput("daterange", "Choose date range:",start="2019-01-01",end="2021-01-01", min="2019-01-01", max="2021-01-01"),
                                                       numericInput("anum1", "ARIMA input 1:", 1),
                                                       numericInput("anum2", "ARIMA input 2:", 1),
                                                       numericInput("anum3", "ARIMA input 3:", 1),
                                                       plotOutput("plot2", height=500, click="plot2_click")),
                                                       verbatimTextOutput("dateText")
                                              )),
                              tabPanel("Multivar. Time Series Prediction",
                                       plotOutput("multiplot", height=500)),
                              tabPanel("Data Inspection",
                                       fluidRow(
                                         selectInput("dataId", label = "Choose dataset", choices = c("mtcars", "mtcars2"),
                                                     selected = NULL, multiple = FALSE)
                                       ),
                                       fluidRow(
                                         column(width = 4,
                                                plotOutput("plot1", height = 300,
                                                           # Equivalent to: click = clickOpts(id = "plot_click")
                                                           click = "plot1_click",
                                                           brush = brushOpts(
                                                             id = "plot1_brush"
                                                           )
                                                ),
                                                plotOutput("plotbrush", height=300, brush=brushOpts(id="plotbrush_brush"))
                                         )
                                       ),
                                       fluidRow(
                                         column(width = 6,
                                                h4("Points near click"),
                                                verbatimTextOutput("click_info")
                                         ),
                                         column(width = 6,
                                                h4("Brushed points"),
                                                verbatimTextOutput("brush_info")
                                         )
                                       )
                              )
)
)

servertabs2 <- function(input, output) {
  
  output$val1 <- renderText({input$num1})
  
  base <- reactive({get(input$dataId)}) 
  
  output$dateText  <- renderText({
    paste("input$date is", as.character(input$daterange))
  })
  
  startdate <- reactive({get(input$daterange[1])})
  enddate <- reactive({get(input$daterange[2])})
  
  # subset the time series data 
  
  date1 <- as.Date(startdate, "%m-%d-%Y")
  date2 <- as.Date(enddate, "%m-%d-%Y")
  
  # need them to have the same column names (?)
  
  #output$plot1 <- renderPlot({
  #  ggplot(datas, aes(wt, mpg)) + geom_point()
  #}) 
  output$plot1 <- renderPlot({
    mydata <- base()
    ggplot(mydata, aes(wt, mpg))+ geom_point()
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
  
  # Arima model library(forecasts)
  amodel <- Arima(tx, order = c(1,2,3))
  amodel1 <- Arima(txsubset, order= c(1,2,3))
  
  # output time series plot
  output$plot2 <- renderPlot({
    # arima forecast
    #plot(forecast(amodel, 5))
    plot(amodel1)
  })
  
  # multivariate ts library(vars)
  x1 <- diffinv(rnorm(500))
  x3 <- diffinv(rnorm(500))
  x2 <- diffinv(rnorm(500))
  x4 <- cbind(x1, x2, x3)
  xts <- as.ts(x4)
  Var.x <- VAR(xts, p=2, type="const")
  fcst.x <- forecast(Var.x)
  
  output$multiplot <- renderPlot({
    plot(fcst.x)
  })
}

# preprocessing 
alabama$day <- as.Date(alabama$date)



