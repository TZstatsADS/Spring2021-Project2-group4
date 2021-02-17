### SHINY SERVER ###

server = function(input, output, session) {
    
    # covid tab 
    formatted_date = reactive({
        format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
    })
    
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(formatted_date()),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == formatted_date())
    })
    
    reactive_db_last7d = reactive({
        cv_cases %>% filter(date == formatted_date() & new_cases>0)
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last7d = reactive({
        large_countries = reactive_db_last7d() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_hemisphere = reactive({
        large_countries = reactive_db_last7d() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
    })
    
    reactive_polygons_last7d = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last7d()$alpha3, ]
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
    })
    
    output$reactive_country_count <- renderText({
        paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
    })
    
    output$reactive_new_cases_7d <- renderText({
        paste0(round((cv_aggregated %>% filter(date == formatted_date() & region=="Global"))$new/7,0), " 7-day average")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                             fillOpacity = 0.2, color = covid_col, group = "COVID-19 (cumulative)",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Avg Temperature: %.1f<br/>Confirmed cases: %g<br/>Cases per million: %g<br/>Weather stations: %g", 
                                             reactive_db()$country, round(reactive_db()$'Average_Temp', 1), reactive_db()$cases, reactive_db()$cases_per_million, reactive_db()$Station_Count) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%  
            
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.5, fillColor = ~cv_pal(reactive_db_large()$'Average_Temp')) %>%
            
            addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases_per_million/50), 
                             fillOpacity = 0.1, color = covid_col, group = "COVID-19 (new)",
                             label = sprintf("<strong>%s (7-day average)</strong><br/>Avg Temperature: %.1f<br/>New cases: %g<br/><b>New cases per million: %g</b><br/>Weather stations: %g", 
                                             reactive_db_last7d()$country, reactive_db_last7d()$'Average_Temp', round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), reactive_db_last7d()$Station_Count) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) 
    })
    
    
    output$cumulative_northern_plot <- renderPlot({
        cumulative_plot(cv_aggregated_northern, formatted_date())
    })
    
    output$cumulative_southern_plot <- renderPlot({
        cumulative_plot(cv_aggregated_southern, formatted_date())
    })
    
    output$epi_northern_curve <- renderPlot({
        new_cases_plot(cv_aggregated_northern, formatted_date(), 'Northern Hemisphere')
    })
    
    output$epi_southern_curve <- renderPlot({
        new_cases_plot(cv_aggregated_southern, formatted_date(), 'Southern Hemisphere')
    })
    
    # add footnote for cases
    output$epi_notes_1 <- renderText({
        if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
    })
    
    # add footnote for deaths
    output$epi_notes_2 <- renderText({
        if(input$comparison_metric=="deaths") { 
            paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
        }
    })
    
    # add note for cfr
    output$epi_notes_3 <- renderText({
        if(input$comparison_metric=="cfr") { 
            paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
        }
    })
    
    # northern new plots
    output$northern_plot_new <- renderPlotly({
        if(input$outcome_select=='Cases') {
            new_cases_plot_varstart(cv_aggregated_northern, input$date_range, "Northern Hemisphere")
        }
        else {
            new_deaths_plot(cv_aggregated_northern, input$date_range, "Northern Hemisphere")
        }
    })
    
    # northern cumulative plots
    output$northern_plot_cumulative <- renderPlotly({
        if(input$outcome_select=='Cases') {
            cumulative_cases_plot(cv_aggregated_northern, input$date_range,"Northern Hemisphere")
        }
        else {
            cumulative_deaths_plot(cv_aggregated_northern, input$date_range, "Northern Hemisphere")
        }
    })
    
    # southern new plots
    output$southern_plot_new <- renderPlotly({
        if(input$outcome_select=='Cases') {
            new_cases_plot_varstart(cv_aggregated_southern, input$date_range, "Southern Hemisphere")
        }
        else {
            new_deaths_plot(cv_aggregated_southern, input$date_range, "Southern Hemisphere")
        }
    })
    
    # southern cumulative plots
    output$southern_plot_cumulative <- renderPlotly({
        if(input$outcome_select=='Cases') {
            cumulative_cases_plot(cv_aggregated_southern, input$date_range, "Southern Hemisphere")
        }
        else {
            cumulative_deaths_plot(cv_aggregated_southern, input$date_range, "Southern Hemisphere")
        }
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            cv_cases_sub = cv_cases %>% dplyr::select(c(country, date, cases, new_cases, deaths, new_deaths,
                                                 cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
            names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                    "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
            write.csv(cv_cases_sub, file)
        }
    )
    
    output$rawtable <- renderPrint({
        cv_cases_sub = cv_cases %>% dplyr::select(c(country, date, cases, new_cases, deaths, new_deaths,
                                             cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
        names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
        orig <- options(width = 1000)
        print(tail(cv_cases_sub, input$maxrows), row.names = FALSE)
        options(orig)
    })
    
    #S add  
    
    # forecast library for arima
    
    # time series transform of cv_cases
    
    # by country
    
    cv_cases1 = cv_cases %>% dplyr::select(date, country, cases, Average_Temp)
    
    # put it in global.R ?
    USA <- cv_cases1[cv_cases1$country=="USA",]
    China <- cv_cases1[cv_cases1$country=="Mainland China",]
    Thailand <- cv_cases1[cv_cases1$country=="Thailand",]
    France <- cv_cases1[cv_cases1$country=="France",]
    Morocco <- cv_cases1[cv_cases1$country=="Morocco",]
    Australia <- cv_cases1[cv_cases1$country=="Australia",]
    Mexico <- cv_cases1[cv_cases1$country=="Mexico",]
    Taiwan <- cv_cases1[cv_cases1$country=="Taiwan",]
    Greece <- cv_cases1[cv_cases1$country=="Greece",]
    Canada <- cv_cases1[cv_cases1$country=="Canada",]
    
    # reactive vals for choice of Country
    base_country1 <- reactive({get(input$countryId1)}) 
    base_country2 <- reactive({get(input$countryId2)})
    base_country3 <- reactive({get(input$countryId3)})
    
    # numeric input for arima
    arima1 <- reactive({as.numeric(input$anum1)})
    arima2 <- reactive({as.numeric(input$anum2)})
    arima3 <- reactive({as.numeric(input$anum3)})

    output$plot1 <- renderPlot({
        mydata1 <- base_country3()
        country_ts <- as.ts(mydata1)
        country_var <- VAR(country_ts, p=1, type="const")
        country_forecast <- predict(country_var, n.ahead=2, ci=0.95)
        plot(country_forecast)        
    })
    
    output$plot2 <- renderPlot({
        # arima forecast
        countrybase_arima <- base_country1()
        arima_cases = countrybase_arima %>% dplyr::select(cases)
        uni_country_ts <- as.ts(arima_cases)
        amodel <- Arima(uni_country_ts, order = c(1,2,3))
        plot(forecast(amodel, 5))
    })

    output$multiplot <- renderPlot({
        # just plot the time series itself
        data_multi <- base_country2()
        data_multi$day = as.Date(data_multi$date)
        ggplot()+
            geom_line(data = data_multi, aes(day,cases), color="blue")+
            xlab("Time")+ylab("Count")+
            geom_line(data= data_multi, aes(day, Average_Temp), color="green",values =c('blue'='blue','green'='green'), labels = c('num. cases','avg. temp.'))
    })

    # time series transform of hosp data
    h_dates <- as.Date(hosp_dat$date_of_interest, "%m/%d/%Y")

    hosp_dat_1 <- data.frame(day = h_dates, hcount = hosp_dat$HOSPITALIZED_COUNT, bcount = hosp_dat$BX_HOSPITALIZED_COUNT)

    hts <- as.ts(hosp_dat_1)

    hvar <- VAR(hts, p=1, type="trend")

    hforecast <- predict(hvar, n.ahead = 5, ci=0.95)

    output$nycplot <- renderPlot({
        fanchart(hforecast, names = "hcount", main="hosp forecast")
    })
}
