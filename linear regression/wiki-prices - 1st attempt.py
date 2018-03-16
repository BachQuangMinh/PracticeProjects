import pandas as pd
import math
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from scipy import stats
import matplotlib.pyplot as plt
#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\linear regression\\WIKI-PRICES.csv')
    #Explore the data
df.isnull().sum()
df.describe()
df.std()
df['open'].hist()
plt.show()
    #Feature selection
df = df[['adj_open','adj_high','adj_low','adj_close','adj_volume']]
df['OC_PercentageChange'] = (df['adj_close']-df['adj_open'])/df['adj_open']*100
df['HL_PercentageChange'] = (df['adj_high']-df['adj_low'])/df['adj_low']*100
df = df[['adj_close', 'HL_PercentageChange', 'OC_PercentageChange', 'adj_volume']]

#df = df[['adj_close', 'HL_PercentageChange', 'OC_PercentageChange']]

#The next questions to ask are: what will be the labels? or classes?
#-->We're trying to predict the closed price (adj-closed)
#-->Then what would be the features? 
#-->they are: the current adj-closed price, HL_PercentageChange, OC_PercentageChange,  adj_volume

forecast_col = 'adj_close' #just a place holder for the label one wants to predict.
#df.fillna(value = -999999999,inplace = True) #just fill in the missing value with an outlier
forecast_out = int(math.ceil(0.001*len(df))) #the number of days in the future we want to predict
df['label'] = df[forecast_col].shift(-forecast_out) 
#shift the forecast_col up 0.001*len(df)
#in this case we just wanna predict 10 next day using the adj_close of previous days.
#df.dropna(inplace=True)

SampleForPrediction = np.array(df[-10:].drop(['label'],1)) #take the last 10 rows for prediction

X = np.array(df.drop(['label'], 1))
y = np.array(df['label'])
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#train and test
clf = LinearRegression(fit_intercept=True)
model = clf.fit(X_train, y_train)
predictions = model.predict(X_train)
betahat = np.append(model.intercept_, model.coef_)

#evaluation
    #Q1: is there a relationship between the response and predictors?
    #create summary for linear regression. Note that this could be done easily if you use stats package.
    #however for the purpose of study, let's do it by hand
newX_train = pd.DataFrame({"Constant":np.ones(len(X_train))}).join(pd.DataFrame(X_train)) #add 1s to the matrix
sigmasquaredhat = sum((y_train - predictions)**2)/(len(newX_train)-len(newX_train.columns))
var_betahat = sigmasquaredhat*(np.linalg.inv(np.dot(newX_train.T,newX_train)).diagonal()) #reference from All of Statistics page 217
sd_betahat = np.sqrt(var_betahat)
tvalue = betahat/(sd_betahat)
pvalue = [2*(1-stats.t.cdf(np.abs(i),(len(newX_train)-len(newX_train.columns)))) for i in tvalue] #reference from Paul slide, confidence interval and test 

sd_betahat = np.round(sd_betahat,3)
tvalue = np.round(tvalue,3)
pvalue = np.round(pvalue,3)
betahat = np.round(betahat,4)
TSS = sum((y_train - y_train.mean())**2)
RSS = sum((y_train - predictions)**2)
F_stat = ((TSS-RSS)/len(df.columns))/(RSS/(len(newX_train)-len(df.columns)-1))

alpha = 0.05
lowerbound_conf_int=np.array([betahat-stats.t.ppf(1-alpha/2,df=len(newX_train)-len(newX_train.columns))*sd_betahat])
upperbound_conf_int=np.array([betahat+stats.t.ppf(1-alpha/2,df=len(newX_train)-len(newX_train.columns))*sd_betahat])
#ppf is percentile 

summary = pd.DataFrame()
summary["Coefficients"],summary["Standard Errors"],summary["t values"],summary["p values"],summary['lower_conf'],summary['upper_conf'] = [betahat,sd_betahat,tvalue,pvalue,lowerbound_conf_int.T,upperbound_conf_int.T]
#summary = summary.rename(index = {0: 'intercept', 1: 'adj_close', 2: 'HL_PCT', 3: 'PCT_Change', 4: 'adj_volume'})
summary = summary.rename(index = {0: 'intercept', 1: 'adj_close', 2: 'HL_PCT', 3: 'PCT_Change'})

print(summary)
print('The F statistics is ' + str(F_stat))
    #the F_stat show that there definitely is a relationship between the response and predictors
    #the pvalue show partially how impactful each feature is (the smaller pvalue the better)
    #An experiment has been made, obmitting 'adj_volume', yields higher rsquared and pvalues are relatively small 
    #this approach is considered as backward selection
    
    #Q2: Select important features 
    #we can perform backward selection (can only be done when n>p), forward selection or mixed selection
    #also to judge the quality of the model, we can also look at AIC, BIC and adjusted Rsquared
    
    #Q3: How well does the model fit the data?
rsquared = model.score(X_test, y_test) 
    #this is actually the r squared, close to 1 is good, close to 0 is bad.
    #this is also the cor(Y,Yhat)^2
    
    #Q4: Given a set of features, how should we pick the predicted response? how accurate is it?
yhat = model.predict(X_test)
X_test = pd.DataFrame({"Constant":np.ones(len(X_test))}).join(pd.DataFrame(X_test)) #add 1s

upper_conf_int_yhat = []
for j in range(0,len(X_test)):
    c = np.array(X_test.ix[0]).T
    c.dot(np.linalg.inv(np.dot(newX_train.T,newX_train))).dot(c)
    upper = yhat[j]+stats.t.ppf(1-alpha/2,df=len(newX_train)-len(newX_train.columns))*np.sqrt(sigmasquaredhat)*c.dot(np.linalg.inv(np.dot(newX_train.T,newX_train))).dot(c) 
    upper_conf_int_yhat.append(upper)

lower_conf_int_yhat = []
for j in range(0,len(X_test)):
    c = np.array(X_test.ix[0]).T
    c.dot(np.linalg.inv(np.dot(newX_train.T,newX_train))).dot(c)
    lower = yhat[j]-stats.t.ppf(1-alpha/2,df=len(newX_train)-len(newX_train.columns))*np.sqrt(sigmasquaredhat)*c.dot(np.linalg.inv(np.dot(newX_train.T,newX_train))).dot(c) 
    lower_conf_int_yhat.append(lower)
    
summary_y = pd.DataFrame()
summary_y['y'],summary_y['y hat'],summary_y['lower_conf'],summary_y['upper_conf']=[y_test,yhat,lower_conf_int_yhat,upper_conf_int_yhat]
    #using residual plots to identify non-linearity
residuals = y_test - yhat
plt.scatter(y_test, residuals)    
plt.show()
    #there's no clear patterns in the residual plot indecating that there is a linear relationship
#Prediction for the next ten days
Predicted_adj_close = model.predict(SampleForPrediction)
compare = pd.DataFrame()
compare['actual'],compare['predicted']=[y[-10:],Predicted_adj_close]
