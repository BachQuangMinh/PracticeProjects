# -*- coding: utf-8 -*-
"""
Created on Sun Apr 22 16:29:23 2018

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

#import the data and preprocess the data
data=pd.read_excel('datasample_airbnb.xlsx')

features=data.loc[:,['Neighbourhood ', 'Property Type', 'Room Type']]
price=data['Price']

propertytypevalues=list(features['Property Type'].unique())
propertytypetogroup=['Bed & Breakfast', 'Boat', 'Bungalow', 'Cabin',
                     'Camper/RV', 'Castle', 'Chalet', 'Condominium', 
                     'Dorm','Hut','Other', 'Tent', 'Townhouse', 
                     'Treehouse', 'Villa','Lighthouse']
for i in propertytypetogroup:
    features.replace(to_replace=i, value='Others', inplace=True)

#Encode the qualitative variables and add the quadratic terms 
from sklearn.preprocessing import LabelEncoder   
number=LabelEncoder()

features.iloc[:,0]=number.fit_transform(features.iloc[:,0])
features.iloc[:,1]=number.fit_transform(features.iloc[:,1])
features.iloc[:,2]=number.fit_transform(features.iloc[:,2])
features['Neighbourhood squared']=features['Neighbourhood ']**2
features['Room Type squared']=features['Room Type']**2

#check for correlation 
sample=pd.concat([features,price],axis=1)
corr=sample.corr()
#Prepare the data for training and testing
from sklearn.model_selection import train_test_split

X = features
y = price
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#Train and evaluate the model
X_train = sm.add_constant(X_train)
model = sm.OLS(y_train,X_train)
result = model.fit()
betahat = result.params
print(result.summary())

yhat_train=model.predict(params=betahat,exog=X_train)
residuals_train = y_train - yhat_train
MAPE_train = np.abs(residuals_train/y_train).mean()
plt.figure(figsize=(6*1.5,3*1.5))
plt.scatter(y_train, residuals_train) 
axes = plt.gca()
axes.set_xlim([0,150])
axes.set_ylim([-400,400])   
plt.grid(True)
plt.title('Residual Plots')
plt.xlabel('Actual Price')
plt.ylabel('Residuals')
plt.show()

#prediction accuracy assessment
Const_X_test=sm.add_constant(X_test)
yhat = model.predict(params=betahat,exog=Const_X_test)
rsquared = np.corrcoef(yhat,y_test)**2

residuals_test = y_test - yhat
MAPE_test=np.abs(residuals_test/y_test).mean()
plt.scatter(y_test, residuals_test)    
axes = plt.gca()
axes.set_xlim([0,150])
axes.set_ylim([-400,400])  
plt.show()