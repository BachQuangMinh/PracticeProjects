# -*- coding: utf-8 -*-
"""
Created on Tue Mar  6 09:14:13 2018

@author: DELL
"""
import numpy as np
import matplotlib.pyplot as plt


data = np.array([45.5,75.6,40.6,66.3,66.7,18.1,58.4,47.1,27.1,52.8,
        47.4,53.0,40.0,17.2,29.8,29.1,43.9,48.7,44.5,27.5,
        50.7,30.9,45.4,46.7,37.5,40.3,40.7,53.2,54.0,41.5])
        #43.4,57.2,38.5,37.9,48.9,34.1,42.3,53.0,38.0,36.5,
        #42.6,31.7,43.0,47.2,56.3,32.8,38.0,46.5,33.2,49.0

def bin_weigh(data, bins):
    "apply weights to the data points according to the bins"
    weights = np.array([])
    for i in data:
        for j in range(len(bins)-1):
            if np.logical_and(i >= bins[j], i < bins[j+1]):
                weight = 1/(bins[j+1]-bins[j])
                weights = np.append(weights,weight)            
    return weights

bins=[10,30,60,70,80]    
weights = bin_weigh(data, bins)        

n, bins, patches = plt.hist(data, bins=bins, range=None, normed=False, weights=weights, cumulative=False,
         bottom=None, histtype='bar', align='mid', orientation='vertical',
         rwidth=None, log=False, color=None, label=None, stacked=True,
         hold=None, data=None)
plt.xlabel('time')
plt.ylabel('frequency density')
plt.show()

classwidth = [bins[j+1]-bins[j] for j in range(len(bins)-1)]
counts = n*classwidth
total = counts.sum()
percentile = np.percentile(data,80)
mean = data.mean()
std = np.std(data)

