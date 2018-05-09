# -*- coding: utf-8 -*-
"""
Created on Fri Apr 20 19:14:05 2018

@author: DELL
"""

import pandas as pd
import statsmodels.api as sm
import numpy as np
import matplotlib.pyplot as plt

#set work dir
dir='C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\airbnb'
import os 
os.chdir(dir)

#import the reviewed data
reviewed=pd.read_excel('reviewed.xlsx')

#how does the price distribute among the neighborhood?
Manhattan_price=reviewed[reviewed.loc[:,'Neighbourhood ']=='Manhattan'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Manhattan_price, bins=30, density=True)
plt.title('Price distribution in Manhattan')
plt.xlabel('Price')
plt.show()

Bronx_price=reviewed[reviewed.loc[:,'Neighbourhood ']=='Bronx'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Bronx_price, bins=30, density=True)
plt.title('Price distribution in Bronx')
plt.xlabel('Price')
plt.show()

Queens_price=reviewed[reviewed.loc[:,'Neighbourhood ']=='Queens'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Queens_price, bins=30, density=True)
plt.title('Price distribution in Queens')
plt.xlabel('Price')
plt.show()

Brooklyn_price=reviewed[reviewed.loc[:,'Neighbourhood ']=='Brooklyn'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Brooklyn_price, bins=30, density=True)
plt.title('Price distribution in Brooklyn')
plt.xlabel('Price')
plt.show()

StatenIsland_price=reviewed[reviewed.loc[:,'Neighbourhood ']=='Staten Island'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(StatenIsland_price, density=True)
plt.title('Price distribution in Staten Island')
plt.xlabel('Price')
plt.show()

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([Manhattan_price,
             Brooklyn_price,
             Queens_price,
             Bronx_price,
             StatenIsland_price],vert=False)
plt.title('Price over view by Neighbourhood')
plt.xlabel('Price')
plt.yticks(range(1,6), ('Manhattan', 'Brooklyn', 'Queens', 'Brox', 'StatenIsland'))
plt.show()