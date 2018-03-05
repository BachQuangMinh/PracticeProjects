import pandas as pd
import math
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from scipy import stats
import matplotlib.pyplot as plt

#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Learn coding\\logistic regression\\breast-cancer-wisconsin.csv')
df['Class'].replace('benign',0, inplace=True)
df['Class'].replace('malignant',1, inplace=True)
df.isnull().values.any()#this gives false, data has no missing value
df.replace('?',10**9,inplace=True)
X = df.drop(['Class','Code'], 1)
y = df['Class']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#Train 
clf = LogisticRegression(penalty='l2', dual=False, tol=0.0001, C=1, fit_intercept=True, intercept_scaling=1, class_weight='balanced', random_state=None, solver='sag', max_iter=10000, multi_class='ovr', verbose=0, warm_start=False, n_jobs=1)
model = clf.fit(X_train, y_train)
trainlable = model.predict(X_train)
betahat = np.append(model.intercept_, model.coef_)

#evaluation
trainscore = model.score(X_train,y_train)
testscore = model.score(X_test, y_test)
#this is pretty bad model, dont know why yet

