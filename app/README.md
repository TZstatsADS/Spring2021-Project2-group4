# Project 2: Shiny App Development
### App folder

The App directory contains the app files for the Shiny App (i.e., ui.R, server.R and global.R).
 - ui.R and server.R are two key components for the Shiny App 
 - global.R is used to preprocess the data and define functions that used in server.R
 - output folder contains the data used for deployment and it will update daily

The temperature overlay for the world map represents the 0th-100th percentile of average temperature (5 bins evenly spaced based on percentile). Northern Hemisphere countries are grouped as countries with latitudes >=15 and <= 75 while Northern Hemisphere countries are grouped as countries with latitudes <=-15 and >= -75. These values were chosen for the purpose of this project; choosing countries at the equator and countries too close to the poles, would mean including countries with little to no seasonal patterns and would hence not be of great purpose to the project. 

The time series models used include ARIMA for univariate time series prediction/forecasting and VAR (vector autoregression) for multivariate time series prediction.

The ARIMA model allows users to choose its three parameters (p,d,q). 

The NYC time series prediction  uses a VAR (vector autoregression) multivariate time series model with p=1 lags where the variables include the date, the citywide count, and the count of hospitalized cases in the Bronx. This tab allows users to choose the period of days in the future that they wish to forecast or predict for number of hospitalizations.   
