# Project 2: Shiny App Development

### [Project Description](doc/project2_desc.md)

![screenshot](doc/figs/map.jpg)

In this second project of GR5243 Applied Data Science, we develop a *Exploratory Data Analysis and Visualization* shiny app on a topic of your choice using [JHU CSSE Covid-19 Data](https://github.com/CSSEGISandData/COVID-19) or NYC Health open data released on the [NYC Coronavirus Disease 2019 Data](https://github.com/nychealth/coronavirus-data) website. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- study legacy codes and further development
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Visualizing WEATHER (or not) Covid Correlates with Temperature
Term: Spring 2021

+ Group 4
+ **Visualizing WEATHER (or not) Covid Correlates with Temperature**:
	+ Serena Yuan
	+ Daryl Kow
	+ Mark Morrissey
	+ Amir Idris

+ **Project summary**: Building an R Shiny app that visualizes temperature in addition to new Covid-19 cases by overlaying the features on a global map, and using time series modelling to predict new cases by previous cases and weather. The Covid-19 case data originates from the Johns Hopkins University Center for Systems Science and Engineering, and the weather data originates from [US National Oceanic and Atmospheric Association's Global Surface Summary of the Day](https://data.noaa.gov/dataset/dataset/global-surface-summary-of-the-day-gsod). The temperature data is displayed using a heat-map, and the new cases are displayed using a bubble chart, both overlayed on a map. The time series predictions are displayed using a line graph. To run the Shiny app locally, go to the app folder and run server.R and then ui.R. 

+ **Contribution statement**: All group members contributed appropriately to the final product. AI found the weather data for the project, aggregated the data across years and stations, and uploaded a dataset that included average metrics across stations per country. MM investigated the possibilities for geospatial separation of weather data, and transformed covid dataset to include 7-day rolling averages. DK found the basic template for the Shiny App and developed it for the purposes of displaying both the weather and covid data, while readying it for publishing. SY investigated and fit a time series model to predict new cases and hospitalizations based on previous cases, deaths, and weather data. The former was included in the final app. DK is the group presenter and presented in class.  

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

