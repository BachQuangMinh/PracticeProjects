import pandas as pd
import math
import numpy as np
from sklearn.model_selection import train_test_split
import statsmodels.api as sm
import matplotlib.pyplot as plt

#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\linear regression\\WIKI-PRICES.csv')
    #Explore the data
df.isnull().sum()
#zero could be denoted as null
#max and min --> check for abnormal values, repeated values 

df.describe()
df.std()
df['open'].hist()
plt.show()

    #Feature selection
#add correlation inspection-->how to add a graph of correlation????
import seaborn as sns
# Compute the correlation matrix
corr = df.corr()

# Generate a mask for the upper triangle
mask = np.zeros_like(corr, dtype=np.bool)
mask[np.triu_indices_from(mask)] = True

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)

# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})

df = df[['adj_open','adj_high','adj_low','adj_close','adj_volume']]
df['OC_PercentageChange'] = (df['adj_close']-df['adj_open'])/df['adj_open']*100
df['HL_PercentageChange'] = (df['adj_high']-df['adj_low'])/df['adj_low']*100
df = df[['adj_close', 'HL_PercentageChange', 'OC_PercentageChange', 'adj_volume']]
#could add more charts to seek for patterns --> what specific charts to add in???
#need to be done more carefully


#The next questions to ask are: what will be the labels? or classes?
#-->We're trying to predict the closed price (adj-closed)
#-->Then what would be the features? 
#-->they are: the current adj-closed price, HL_PercentageChange, OC_PercentageChange,  adj_volume

forecast_col = 'adj_close' #just a place holder for the label one wants to predict.
forecast_out = int(math.ceil(0.001*len(df))) #the number of days in the future we want to predict
df['label'] = df[forecast_col].shift(-forecast_out) 
#shift the forecast_col up 0.001*len(df)
#in this case we just wanna predict 10 next day using the adj_close of previous days.

SampleForPrediction = np.array(df[-10:].drop(['label'],1)) #take the last 10 rows for prediction
df.dropna(inplace=True)

X = np.array(df.drop(['label'], 1))
y = np.array(df['label'])
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#train and test
X_train = sm.add_constant(X_train)
model = sm.OLS(y_train,X_train)
result = model.fit()
betahat = result.params


#evaluation
    #Q1: is there a relationship between the response and predictors?
print(result.summary())
    
    #the F_stat show that there definitely is a relationship between the response and predictors
    #-->see Introduction to Statistical learning page 75
    #the pvalue show partially how impactful each feature is (the smaller pvalue the better)
    #An experiment has been made, obmitting 'adj_volume', yields higher rsquared and pvalues are relatively small 
    #this approach is considered as backward selection
    
    #Q2: Select important features 
    #we can perform backward selection (can only be done when n>p), forward selection or mixed selection
    #also to judge the quality of the model, we can also look at AIC, BIC and adjusted Rsquared
    
    #Q3: How well does the model fit the data?
    #Look at the r squared, close to 1 is good, close to 0 is bad.
    #this is also the [[correlation of (Y,Yhat)]^2
    
    #Q4: How well will the model perform on the set test?
    #Get the prediction intervals of the model-->how?(IMPORTANT)
Const_X_test = pd.DataFrame({"Constant":np.ones(len(X_test))}).join(pd.DataFrame(X_test)) #add 1s to the matrix
yhat = model.predict(params=betahat,exog=Const_X_test)
rsquared = np.corrcoef(yhat,y_test)**2
predictobservation = pd.DataFrame({'yhat': yhat, 'ytest': y_test})

#Potential problems
#1. Non-linearity of the response-predictor relationships.
#2. Correlation of error terms.
#3. Non-constant variance of error terms.
#4. Outliers.
#5. High-leverage points.
#6. Collinearity --> check on this more 
    
    #1. using residual plots to identify non-linearity
residuals = y_test - yhat
plt.scatter(y_test, residuals)    
plt.show()
    #there's no clear patterns in the residual plot indecating that there is a linear relationship
    #-->see Introduction to Statistical learning page 93

#Prediction for the next ten days
Const_SampleForPrediction = pd.DataFrame({"Constant":np.ones(len(SampleForPrediction))}).join(pd.DataFrame(SampleForPrediction)) #add 1s to the matrix
Predicted_adj_close = model.predict(params=betahat,exog=Const_SampleForPrediction)

