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
        #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last7d = reactive({
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
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5.5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%  
            
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_million)) %>%
            
            addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5.5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                             label = sprintf("<strong>%s (7-day average)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db_last7d()$country, round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_deaths/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), round(reactive_db_last7d()$new_deaths_per_million/7,1)) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) 
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, formatted_date())
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(cv_aggregated, formatted_date())
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
    
    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Global") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = "Global", selected = "Global")
        }
        
        if (input$level_select=="Continent") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                              selected = c("Africa", "Asia", "Europe", "North America", "South America"))
        }
        
        if (input$level_select=="US state") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_states_today[order(-cv_states_today$cases),]$state), 
                              selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
        }
        
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                              selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Global") { 
            db = cv_cases_global
            db$region = db$global_level
        }
        if (input$level_select=="Continent") { 
            db = cv_cases_continent 
            db$region = db$continent
        }
        if (input$level_select=="Country") { 
            db = cv_cases
            db$region = db$country
        }
        if (input$level_select=="US state") { 
            db = cv_states
            db$region = db$state
        }
        
        if (input$outcome_select=="Cases (total)") { 
            db$outcome = db$cases
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths (total)") { 
            db$outcome = db$deaths 
            db$new_outcome = db$new_deaths 
        }
        
        if (input$outcome_select=="Cases per million") { 
            db$outcome = db$cases_per_million 
            db$new_outcome = db$new_cases_per_million 
        }
        
        if (input$outcome_select=="Deaths per million") { 
            db$outcome = db$deaths_per_million 
            db$new_outcome = db$new_deaths_per_million 
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    # country-specific plots
    output$country_plot <- renderPlotly({
        country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative <- renderPlotly({
        country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative_log <- renderPlotly({
        country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            cv_cases_sub = cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                                 cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
            names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                    "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
            write.csv(cv_cases_sub, file)
        }
    )
    
    output$rawtable <- renderPrint({
        cv_cases_sub = cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                             cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
        names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
        orig <- options(width = 1000)
        print(tail(cv_cases_sub, input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}