
uitabs <- shinyUI( navbarPage("Weather and Covid", id="nav", theme="bootstrap.css",
                              tabPanel("Interactive Map",
                                       fluidRow(
                                         numericInput("num1", "Input number:", 1),
                                         verbatimTextOutput("val1")
                                       )),
                              tabPanel("Time Series Visualizations",
                                       fluidRow(column(width=4,
                                                       plotOutput("plot2", height=350, click="plot2_click")))),
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
                                                )
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
  
  # add the time series analysis
  
  # Arima model library(forecasts)
  amodel <- Arima(hos_ts, order = c(1,2,3))
  
  # output time series plot
  output$plot2 <- renderPlot({
    # arima forecast
    plot(forecast(amodel, 5))
  })
  
}
