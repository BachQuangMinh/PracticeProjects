# -*- coding: utf-8 -*-
"""
Created on Sun Mar 25 13:25:22 2018

@author: DELL
"""

# -*- coding: utf-8 -*-
"""
Created on Sat Mar 24 08:49:38 2018

@author: DELL
"""

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams
from statsmodels.tsa.stattools import acf, pacf
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.ar_model import AR

dir='C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\Time series project\\energydata_complete.csv'
rawdata=pd.read_csv(dir, index_col='date')
timeseries=pd.DataFrame(rawdata['Appliances'])
timeseries.index=pd.to_datetime(timeseries.index)

#summary
timeseries.describe()

#Plot the first 10 days of the time series
fig1=plt.figure(1)
plt.plot(timeseries['2016-01-12':'2016-01-21'])
plt.xlabel('Time')
plt.ylabel('Energy(Wh)')
plt.title('Energy Consumption - The first ten days')
plt.show()
plt.close(fig1)

#Plot the transformation of the time series
fig2=plt.figure(2)
plt.plot(np.log10(timeseries['2016-01-12':'2016-01-21']))
plt.xlabel('Time')
plt.ylabel('Log10 Energy(Wh)')
plt.title('Log Energy Consumption - The first ten days')
plt.show()
plt.close(fig2)

#add log scale
timeseries['Log scale']=np.log10(timeseries['Appliances'])

#samples for analysis: the first 10 days of the time series in log scale
logsample=timeseries.loc['2016-01-12':'2016-01-21','Log scale']

#plot ACF with limits for the test that the value is 0
lag_acf = acf(logsample, nlags=1440)
fig3=plt.figure(3)
plt.plot(lag_acf)
plt.axhline(y=0,linestyle='--',color='gray')
plt.axhline(y=-1.96/np.sqrt(len(logsample)),linestyle='--',color='gray')
plt.axhline(y=1.96/np.sqrt(len(logsample)),linestyle='--',color='gray')
plt.title('Autocorrelation Function')
plt.xticks(np.arange(0, len(logsample), 144))
plt.show()

#plot PACF with limits for the test that the value is 0
lag_pacf = pacf(logsample, nlags=1439, method='ols')
fig4=plt.figure(4)
plt.plot(lag_pacf)
plt.axhline(y=0,linestyle='--',color='gray')
plt.axhline(y=-1.96/np.sqrt(len(logsample)),linestyle='--',color='gray')
plt.axhline(y=1.96/np.sqrt(len(logsample)),linestyle='--',color='gray')
plt.title('Partial Autocorrelation Function')
plt.xticks(np.arange(0, len(logsample), 144))
plt.show()

#find the highest-autocorrelation lag
highest_pacf_lag=np.argmax(lag_pacf[1:],axis=0)+1

#fit the AR(1) model
ar_1=AR(logsample)
ar_1model=ar_1.fit(maxlag=1)
ar_1pred=ar_1.predict
