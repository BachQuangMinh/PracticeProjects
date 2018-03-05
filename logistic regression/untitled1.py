import pandas as pd
import statsmodels.api as sm
import numpy as np
from sklearn.model_selection import train_test_split
import pylab as pl

#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Learn coding\\logistic regression\\breast-cancer-wisconsin.csv')
    #explore the data
df.describe()
df.std()
#tempt = pd.crosstab(df['ClumpThickness'], df['Class'])
df.hist()
pl.show()
    #prepare the data
df['Class'].replace('benign',2, inplace=True)
df['Class'].replace('malignant',4, inplace=True)
df.replace('?',10**9,inplace=True)
df.isnull().values.any()#this gives false, data has no missing value
X = df.drop(['Class','Code'], 1)
X = pd.DataFrame({"Constant":np.ones(len(X))}).join(pd.DataFrame(X))
y = df['Class']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)
X_train.astype(int)
y_train.astype(int)
X_train = np.array(X_train)
y_train = np.array(y_train)

#Train 
clf = sm.Logit(y_train, X_train)
model = clf.fit(X_train, y_train)

trainlable = model.predict(X_train)
betahat = np.append(model.intercept_, model.coef_)

#evaluation
trainscore = model.score(X_train,y_train)
testscore = model.score(X_test, y_test)
#this is pretty bad model, dont know why yet