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

dir='C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\Time series project\\energydata_complete.csv'
rawdata=pd.read_csv(dir, index_col='date')
timeseries=pd.DataFrame(rawdata['Appliances'])
timeseries.index=pd.to_datetime(timeseries.index)



#add log scale
timeseries['Log scale']=np.log10(timeseries['Appliances'])

#samples for analysis: the first 10 days of the time series in log scale
logsample=timeseries.loc['2016-01-12':'2016-01-21','Log scale']

#plot ACF with limits for the test that the value is 0


from pandas import Series
from matplotlib import pyplot
from statsmodels.graphics.tsaplots import plot_acf
plot_acf(logsample, lags=1000)
pyplot.show()