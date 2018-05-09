# -*- coding: utf-8 -*-
"""
Created on Sat Apr 21 11:05:33 2018

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

#import the data 
data=pd.read_excel('datasample_airbnb.xlsx')

#how does the price distribute among the neighborhood?
Manhattan_price=data[data.loc[:,'Neighbourhood ']=='Manhattan'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Manhattan_price, bins=100, density=True)
plt.title('Price distribution in Manhattan')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

Bronx_price=data[data.loc[:,'Neighbourhood ']=='Bronx'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Bronx_price, bins=100, density=True)
plt.title('Price distribution in Bronx')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

Queens_price=data[data.loc[:,'Neighbourhood ']=='Queens'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Queens_price, bins=100, density=True)
plt.title('Price distribution in Queens')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

Brooklyn_price=data[data.loc[:,'Neighbourhood ']=='Brooklyn'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Brooklyn_price, bins=100, density=True)
plt.title('Price distribution in Brooklyn')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

StatenIsland_price=data[data.loc[:,'Neighbourhood ']=='Staten Island'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(StatenIsland_price, density=True, bins=100)
plt.title('Price distribution in Staten Island')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(data.loc[:,'Price'], density=True, bins=100, color='green')
plt.title('Overall Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([Manhattan_price,
             Brooklyn_price,
             Queens_price,
             Bronx_price,
             StatenIsland_price,
             data.loc[:,'Price']],vert=False)
plt.title('Price Distribution Overview')
plt.xlabel('Price')
plt.ylabel('Neighbourhood')
plt.yticks(range(1,7), ('Manhattan', 'Brooklyn', 'Queens', 'Bronx', 'StatenIsland','Overall'))
plt.xticks(np.arange(0,10500,500),np.arange(0,11000,500))
plt.show()

Pricedescribe=pd.DataFrame({'Manhattan':Manhattan_price.describe(),
                            'Brooklyn':Brooklyn_price.describe(),
                            'Queens':Queens_price.describe(),
                            'Bronx':Bronx_price.describe(),
                            'StatenIsland':StatenIsland_price.describe(),
                            'Overall':data.loc[:,'Price'].describe()})
cols = Pricedescribe.columns.tolist()
cols = ['Brooklyn', 'Bronx', 'Manhattan', 'Queens', 'StatenIsland', 'Overall']
Pricedescribe=Pricedescribe[cols]

#how does the price distribute among property types?
Apartment_price=data[data.loc[:,'Property Type']=='Apartment'].loc[:,'Price']

House_price=data[data.loc[:,'Property Type']=='House'].loc[:,'Price']

Loft_price=data[data.loc[:,'Property Type']=='Loft'].loc[:,'Price']

a=['Apartment','House','Loft']
tempt=data
for i in a:
    Others_price=tempt[tempt.loc[:,'Property Type']!=i]
    tempt=Others_price   
Others_price=Others_price.loc[:,'Price']    

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Apartment_price, density=True, bins=100)
plt.title('Apartment Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(House_price, density=True, bins=100)
plt.title('House Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Loft_price, density=True, bins=100)
plt.title('Loft Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Others_price, density=True, bins=100)
plt.title('Others Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([House_price,
             Apartment_price,
             Loft_price,
             Others_price,
             data.loc[:,'Price']],vert=False)
plt.title('Price Distribution by Property Type')
plt.xlabel('Price')
plt.ylabel('Property Type')
plt.yticks(range(1,6), ('House', 'Apartment', 'Loft', 'Others','Overall'))
plt.xticks(np.arange(0,10500,500),np.arange(0,11000,500))
plt.show()

PricedescribePropertytype=pd.DataFrame({'Apartment':Apartment_price.describe(),
                            'House':House_price.describe(),
                            'Loft':Loft_price.describe(),
                            'Others':Others_price.describe(),
                            'Overall':data.loc[:,'Price'].describe()})
cols = ['Apartment','House','Loft','Others','Overall']
PricedescribePropertytype=PricedescribePropertytype[cols]

#how does the price distribute among Room Type?
Entire_price=data[data.loc[:,'Room Type']=='Entire home/apt'].loc[:,'Price']
Private_price=data[data.loc[:,'Room Type']=='Private room'].loc[:,'Price']
Shared_price=data[data.loc[:,'Room Type']=='Shared room'].loc[:,'Price']

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Entire_price, density=True, bins=100)
plt.title('Entire home/apt Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Private_price, density=True, bins=100)
plt.title('Private room Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(4*1.5,3*1.5))
plt.hist(Shared_price, density=True, bins=100)
plt.title('Shared room Price distribution')
plt.xlabel('Price')
plt.ylabel('Density')
plt.show()

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([Entire_price,
             Private_price,
             Shared_price,
             data.loc[:,'Price']],vert=False)
plt.title('Price Distribution by Room Type')
plt.xlabel('Price')
plt.ylabel('Property Type')
plt.yticks(range(1,5), ('Entire home/apt', 'Private room', 'Shared room', 'Overall'))
plt.xticks(np.arange(0,10500,500),np.arange(0,11000,500))
plt.show()

PricedescribeRoomtype=pd.DataFrame({'Entire home/apt':Entire_price.describe(),
                            'Private room':Private_price.describe(),
                            'Shared room':Shared_price.describe(),
                            'Overall':data.loc[:,'Price'].describe()})
cols = ['Entire home/apt','Private room','Shared room','Overall']
PricedescribeRoomtype=PricedescribeRoomtype[cols]

