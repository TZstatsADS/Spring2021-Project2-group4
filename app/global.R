# update data with automated script
# source("jhu_data_daily_cases.R") # option to update daily cases
# source("jhu_data_weekly_cases.R") # run locally to update numbers, but not live on Rstudio server /Users/epp11/Dropbox (VERG)/GitHub/nCoV_tracker/app.R(to avoid possible errors on auto-updates)
# source("ny_data_us.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

#--------------------------------------------------------------------
###############################Install Related Packages #######################
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DBI)) install.packages("DBI", repos = "http://cran.us.r-project.org")

#--------------------------------------------------------------------
###############################Setting color#######################
# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"

#--------------------------------------------------------------------
###############################Import data#######################
cv_cases = read.csv("data/input_data/coronavirus.csv")
countries = read.csv("data/input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("data/input_data/50m.geojson", what = "sp")
country_geoms = read.csv("data/input_data/country_geoms.csv")
cv_states = read.csv("data/input_data/coronavirus_states.csv")
hosp_dat <- read.csv("data/input_data/hosp-by-day.csv")

#--------------------------------------------------------------------
###############################Define Functions#######################
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative cases") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date, plot_title) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new_cases, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5)) +
    ggtitle(plot_title) 
  g1
}

# function to plot cumulative cases by date with variable start date
cumulative_cases_plot = function(cv_aggregated, start_date, plot_title) {
  g = ggplot(cv_aggregated, aes(x = date, y = cases, group = 1,
                           text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",cases))) +
    xlim(c(start_date,(current_date+1))) + xlab("Date")
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cases") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10)) +
    ggtitle(plot_title) 
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative deaths by date with variable start date
cumulative_deaths_plot = function(cv_aggregated, start_date, plot_title) {
  g = ggplot(cv_aggregated, aes(x = date, y = deaths, group = 1,
                                text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",cases))) +
    xlim(c(start_date,(current_date+1))) + xlab("Date")
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Deaths") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10)) +
    ggtitle(plot_title) 
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot new cases by date with variable start date
new_cases_plot_varstart = function(cv_aggregated, start_date, plot_title) {
  g = ggplot(cv_aggregated, aes(x = date, y = new_cases, group = 1,
                                text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",cases))) +
    xlim(c(start_date,(current_date+1))) + xlab("Date")
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cases") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10)) +
    ggtitle(plot_title) 
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot new deaths by date with variable start date
new_deaths_plot = function(cv_aggregated, start_date, plot_title) {
  g = ggplot(cv_aggregated, aes(x = date, y = new_deaths, group = 1,
                                text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",cases))) +
    xlim(c(start_date,(current_date+1))) + xlab("Date")
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Deaths") + theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10)) +
    ggtitle(plot_title) 
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

#--------------------------------------------------------------------
###############################DATA PROCESSING: COVID-19#######################

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$cases_per_million = as.numeric(format(round(cv_cases$cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_cases_per_million = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_cases$deaths_per_million = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_deaths_per_million = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/1000000),1),nsmall=1))

# add variable for weeks since 100th case and 10th death
cv_cases$weeks_since_case100 = cv_cases$weeks_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$weeks_since_case100[country_db$cases>=100] = 0:(sum(country_db$cases>=100)-1)
  country_db$weeks_since_death10[country_db$deaths>=10] = 0:(sum(country_db$deaths>=10)-1)
  cv_cases$weeks_since_case100[cv_cases$country==country_name] = country_db$weeks_since_case100
  cv_cases$weeks_since_death10[cv_cases$country==country_name] = country_db$weeks_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset of state data for today's data
if (any(grepl("/", cv_states$date))) { 
  cv_states$date = format(as.Date(cv_states$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_states$date = as.Date(cv_states$date, format="%Y-%m-%d") }
cv_states_today = subset(cv_states, date==max(cv_states$date))

# create subset for countries with at least 1000 cases
cv_today_reduced = subset(cv_today, cases>=1000)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                cases_per_million, new_cases_per_million,
                                deaths_per_million, new_deaths_per_million,
                                weeks_since_case100, weeks_since_death10)), "output/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for weeks since 100th case and 10th death
cv_cases_continent$weeks_since_case100 = cv_cases_continent$weeks_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$weeks_since_case100[continent_db$cases>=100] = 0:(sum(continent_db$cases>=100)-1)
  continent_db$weeks_since_death10[continent_db$deaths>=10] = 0:(sum(continent_db$deaths>=10)-1)
  cv_cases_continent$weeks_since_case100[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_case100
  cv_cases_continent$weeks_since_death10[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_death10
}

# add continent populations
cv_cases_continent$pop = NA
cv_cases_continent$pop[cv_cases_continent$continent=="Africa"] = 1.2e9
cv_cases_continent$pop[cv_cases_continent$continent=="Asia"] = 4.5e9
cv_cases_continent$pop[cv_cases_continent$continent=="Europe"] = 7.4e8
cv_cases_continent$pop[cv_cases_continent$continent=="North America"] = 5.8e8
cv_cases_continent$pop[cv_cases_continent$continent=="Oceania"] = 3.8e7
cv_cases_continent$pop[cv_cases_continent$continent=="South America"] = 4.2e8

# split by hemisphere
cv_cases_northern = cv_cases[cv_cases$latitude >= 15 & cv_cases$latitude <= 60, ]
cv_cases_southern = cv_cases[cv_cases$latitude <= -15 & cv_cases$latitude >= -60, ]

# add normalised counts
cv_cases_continent$cases_per_million =  as.numeric(format(round(cv_cases_continent$cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_cases_per_million =  as.numeric(format(round(cv_cases_continent$new_cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$deaths_per_million =  as.numeric(format(round(cv_cases_continent$deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_deaths_per_million =  as.numeric(format(round(cv_cases_continent$new_deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
write.csv(cv_cases_continent, "output/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$weeks_since_case100 = cv_cases_global$weeks_since_death10 = 0:(nrow(cv_cases_global)-1)

# add normalised counts
cv_cases_global$pop = 7.6e9
cv_cases_global$cases_per_million =  as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_cases_per_million =  as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$deaths_per_million =  as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_deaths_per_million =  as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
write.csv(cv_cases_global, "output/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(-19, 51, 70, 75, 80, 107) # manual based on percentiles
rb_rev <- rev(brewer.pal(6, "RdYlBu"))
cv_pal <- colorBin(rb_rev, domain = cv_large_countries$cases_per_million, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# create cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("COVID-19 (new)", "COVID-19 (cumulative)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("COVID-19 (new)")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
            title = "<small>Temperature</small>") 

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases[c("cases","deaths")], by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases", "deaths")
cv_aggregated_northern = aggregate(cv_cases_northern[c("cases","deaths")], by=list(cv_cases_northern$date), FUN=sum)
names(cv_aggregated_northern) = c("date", "cases", "deaths")
cv_aggregated_southern = aggregate(cv_cases_southern[c("cases","deaths")], by=list(Category=cv_cases_southern$date), FUN=sum)
names(cv_aggregated_southern) = c("date", "cases", "deaths")

# add variable for new cases in last 7 days
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new_cases[i] = 0 }
  if (i>1) { cv_aggregated$new_cases[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new_deaths[i] = 0 }
  if (i>1) { cv_aggregated$new_deaths[i] = cv_aggregated$deaths[i] - cv_aggregated$deaths[i-1] }
}

for (i in 1:nrow(cv_aggregated_northern)) { 
  if (i==1) { cv_aggregated_northern$new_cases[i] = 0 }
  if (i>1) { cv_aggregated_northern$new_cases[i] = cv_aggregated_northern$cases[i] - cv_aggregated_northern$cases[i-1] }
}
for (i in 1:nrow(cv_aggregated_northern)) { 
  if (i==1) { cv_aggregated_northern$new_deaths[i] = 0 }
  if (i>1) { cv_aggregated_northern$new_deaths[i] = cv_aggregated_northern$deaths[i] - cv_aggregated_northern$deaths[i-1] }
}

for (i in 1:nrow(cv_aggregated_southern)) { 
  if (i==1) { cv_aggregated_southern$new_cases[i] = 0 }
  if (i>1) { cv_aggregated_southern$new_cases[i] = cv_aggregated_southern$cases[i] - cv_aggregated_southern$cases[i-1] }
}
for (i in 1:nrow(cv_aggregated_southern)) { 
  if (i==1) { cv_aggregated_southern$new_deaths[i] = 0 }
  if (i>1) { cv_aggregated_southern$new_deaths[i] = cv_aggregated_southern$deaths[i] - cv_aggregated_southern$deaths[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

cv_aggregated_northern$region = "Northern"
cv_aggregated_northern$date = as.Date(cv_aggregated_northern$date,"%Y-%m-%d")

cv_aggregated_southern$region = "Southern"
cv_aggregated_southern$date = as.Date(cv_aggregated_southern$date,"%Y-%m-%d")

# 
# assign colours to countries to ensure consistency between plots
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

# add weather data 
weather_con <- DBI::dbConnect(RSQLite::SQLite(), "output/weather_data.sqlite")
weather <- dbSendQuery(conn = weather_con, statement = "SELECT * from weather_transformed_grouped")
df_weather <- dbFetch(weather)
df_weather <- df_weather %>% 
  rename(
    date = DATE,
    alpha2 = Country
  )
df_weather <- subset(df_weather, select = c('date', 'alpha2', 'Average_Temp', 'Station_Count'))
cv_cases <- merge(cv_cases, df_weather, by=c("date","alpha2"))
rm(df_weather)
