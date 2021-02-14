# -*- coding: utf-8 -*-
"""
Created on Sat Feb 13 18:32:04 2021

@author: morrismr
"""

import pandas as pd
import numpy as np
#WILL CHANGE THIS TO R TO SIMPLIFY REPRODUCIBILITY--WANTED TO GET FORMULA DOWN FIRST
# IN PYTHON
# cor_full = pd.read_csv(r'C:\Users\morrismr\Documents\GitHub\Spring2021-Project2-group4\data\input_data\coronavirus_full.csv')
cor_full = pd.read_csv(r'../data/input_data/coronavirus_full.csv')



cor_full.sort_values(by=['jhu_ID', 'date'], inplace=True)

df = cor_full.copy()



df['Date'] = pd.to_datetime(df['date'])
df2 = df.groupby(['jhu_ID', pd.Grouper(key='Date', freq='W-MON')])['new_cases', 'new_deaths'].sum().reset_index().sort_values('Date')
df2.sort_values(by=['jhu_ID', 'Date'], inplace=True)


df2['new_cases'] = df2['new_cases']
df2['new_deaths'] = df2['new_deaths']/7.

df2['new_cases_pct_change'] = df2['new_cases'].pct_change(periods=1)
df2['new_deaths_pct_change'] = df2['new_deaths'].pct_change(periods=1)


df2['regular_date'] = df2['Date'].dt.date

df2.to_csv('Covid_Cases_Rolling_Increases.csv')




df3 = df.groupby('jhu_ID')['new_cases', 'new_deaths'].transform(lambda x: x.rolling(7,1).mean())
df3.rename(columns={'new_cases': 'new_cases_rolling_7', 'new_deaths': 'new_deaths_rolling_7'}, inplace=True)
df2['rolling_7']  = df2['new_cases'].rolling(window=7)

df4 = pd.concat([df, df3], axis=1)
df4 = df4[['jhu_ID', 'date', 'cases', 'deaths', 'new_cases', 'new_deaths', 'new_cases_rolling_7', 'new_deaths_rolling_7' ]]

df4['new_cases_pct_change'] = df4['new_cases_rolling_7'].pct_change(periods=7)
df4['new_deaths_pct_change'] = df4['new_deaths_rolling_7'].pct_change(periods=7)

df4.replace([np.inf, -np.inf], 0, inplace=True)
df4.fillna(0, inplace=True)
df4.sort_values(by=['jhu_ID', 'date'], inplace=True)

df4.to_csv('Improved_Covid_Cases_Rolling_Increases.csv')









