# -*- coding: utf-8 -*-
"""
Created on Thu Apr 19 14:51:42 2018

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
df0=pd.read_excel('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\airbnb\\datasample_airbnb.xlsx')
df0.describe()

#replace nan records with 0. This has been checked
df0.replace(np.nan,0,inplace=True)

#unreviewed records
unreviewed=df0[df0.loc[:,'Review Scores Rating']==0]

#reviewed records
reviewed=df0[df0.loc[:,'Review Scores Rating']!=0]

#split into excel files
writer = pd.ExcelWriter('reviewed.xlsx')
reviewed.to_excel(writer)
writer.save()

writer = pd.ExcelWriter('unreviewed.xlsx')
unreviewed.to_excel(writer)
writer.save()

#explore the reviewed
plt.figure(figsize=(4*1.5,3*1.5))
reviewed.hist()
plt.show()

#How does the price distribute among neighborhood?
