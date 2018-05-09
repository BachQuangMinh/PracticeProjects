# -*- coding: utf-8 -*-
"""
Created on Mon Apr 23 19:22:10 2018

@author: DELL
"""

import pandas as pd
import statsmodels.api as sm
import numpy as np
import matplotlib.pyplot as plt
import itertools
#confusion matrix plot function
def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)
    
    plt.figure(figsize=(9*1.5,3*1.5))
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
#set work dir
dir='C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\airbnb'
import os 
os.chdir(dir)

#import the data and preprocess the data
data=pd.read_excel('reviewed.xlsx')

#how the review score distribute
plt.figure(figsize=(6*1.5,3*1.5))
plt.hist(data.loc[:,'Review Scores Rating'], bins=30, density=True)
plt.title('Score Distribution')
plt.xlabel('Score')
plt.ylabel('Density')
plt.show()

Manhattan_reviewscore=data[data.loc[:,'Neighbourhood ']=='Manhattan'].loc[:,'Review Scores Rating']
Bronx_reviewscore=data[data.loc[:,'Neighbourhood ']=='Bronx'].loc[:,'Review Scores Rating']
Queens_reviewscore=data[data.loc[:,'Neighbourhood ']=='Queens'].loc[:,'Review Scores Rating']
Brooklyn_reviewscore=data[data.loc[:,'Neighbourhood ']=='Brooklyn'].loc[:,'Review Scores Rating']
StatenIsland_reviewscore=data[data.loc[:,'Neighbourhood ']=='Staten Island'].loc[:,'Review Scores Rating']

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([Manhattan_reviewscore,
             Brooklyn_reviewscore,
             Queens_reviewscore,
             Bronx_reviewscore,
             StatenIsland_reviewscore,
             data.loc[:,'Review Scores Rating']],vert=False)
plt.title('Review Scores Distribution by Neighbourhood')
plt.xlabel('Review Scores Rating')
plt.ylabel('Neighbourhood')
plt.yticks(range(1,7), ('Manhattan', 'Brooklyn', 'Queens', 'Bronx', 'StatenIsland','Overall'))
plt.show()

reviewscoredescribeneighbourhood=pd.DataFrame({'Manhattan':Manhattan_reviewscore.describe(),
                            'Brooklyn':Brooklyn_reviewscore.describe(),
                            'Queens':Queens_reviewscore.describe(),
                            'Bronx':Bronx_reviewscore.describe(),
                            'StatenIsland':StatenIsland_reviewscore.describe(),
                            'Overall':data.loc[:,'Review Scores Rating'].describe()})
cols = reviewscoredescribeneighbourhood.columns.tolist()
cols = ['Brooklyn', 'Bronx', 'Manhattan', 'Queens', 'StatenIsland', 'Overall']
reviewscoredescribeneighbourhood=reviewscoredescribeneighbourhood[cols]

Apartment_reviewscore=data[data.loc[:,'Property Type']=='Apartment'].loc[:,'Review Scores Rating']
House_reviewscore=data[data.loc[:,'Property Type']=='House'].loc[:,'Review Scores Rating']
Loft_reviewscore=data[data.loc[:,'Property Type']=='Loft'].loc[:,'Review Scores Rating']

a=['Apartment','House','Loft']
tempt=data
for i in a:
    Others_reviewscore=tempt[tempt.loc[:,'Property Type']!=i]
    tempt=Others_reviewscore   
Others_reviewscore=Others_reviewscore.loc[:,'Review Scores Rating']    

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([House_reviewscore,
             Apartment_reviewscore,
             Loft_reviewscore,
             Others_reviewscore,
             data.loc[:,'Review Scores Rating']],vert=False)
plt.title('Review Scores Distribution by Property Type')
plt.xlabel('Review Scores Rating')
plt.ylabel('Property Type')
plt.yticks(range(1,6), ('House', 'Apartment', 'Loft', 'Others','Overall'))
plt.show()

reviewdescribePropertytype=pd.DataFrame({'Apartment':Apartment_reviewscore.describe(),
                            'House':House_reviewscore.describe(),
                            'Loft':Loft_reviewscore.describe(),
                            'Others':Others_reviewscore.describe(),
                            'Overall':data.loc[:,'Review Scores Rating'].describe()})
cols = ['Apartment','House','Loft','Others','Overall']
reviewdescribePropertytype=reviewdescribePropertytype[cols]

Entire_reviewscore=data[data.loc[:,'Room Type']=='Entire home/apt'].loc[:,'Review Scores Rating']
Private_reviewscore=data[data.loc[:,'Room Type']=='Private room'].loc[:,'Review Scores Rating']
Shared_reviewscore=data[data.loc[:,'Room Type']=='Shared room'].loc[:,'Review Scores Rating']

plt.figure(figsize=(9*1.5,3*1.5))
plt.boxplot([Entire_reviewscore,
             Private_reviewscore,
             Shared_reviewscore,
             data.loc[:,'Review Scores Rating']],vert=False)
plt.title('Review Scores Distribution by Room Type')
plt.xlabel('Review Scores Rating')
plt.ylabel('Room Type')
plt.yticks(range(1,5), ('Entire home/apt', 'Private room', 'Shared room', 'Overall'))
plt.show()

reviewscoredescribeRoomtype=pd.DataFrame({'Entire home/apt':Entire_reviewscore.describe(),
                            'Private room':Private_reviewscore.describe(),
                            'Shared room':Shared_reviewscore.describe(),
                            'Overall':data.loc[:,'Review Scores Rating'].describe()})
cols = ['Entire home/apt','Private room','Shared room','Overall']
reviewscoredescribeRoomtype=reviewscoredescribeRoomtype[cols]

# deal with imbalanced class upsampling minority class
from sklearn.utils import resample
# Separate majority and minority classes
file_A = data[data['Review Score Rating Bin']=='A']
file_B = data[data['Review Score Rating Bin']=='B']
file_C = data[data['Review Score Rating Bin']=='C'] 

# Upsample minority class
file_B_upsampled = resample(file_B, replace=True,     # sample with replacement
                            n_samples=8122,    # to match majority class
                            random_state=123) # reproducible results
file_C_upsampled = resample(file_C, replace=True,     # sample with replacement
                            n_samples=10273,    # to match majority class
                            random_state=123) # reproducible results
# Combine majority class with upsampled minority class
file_upsampled = pd.concat([file_A, file_B, file_C_upsampled])

features=file_upsampled.loc[:,['Price','Neighbourhood ','Property Type','Room Type']]
features=data.loc[:,['Price','Neighbourhood ','Property Type','Room Type']]
propertytypetogroup=['Bed & Breakfast', 'Boat', 'Bungalow', 'Cabin',
                     'Camper/RV', 'Castle', 'Chalet', 'Condominium', 
                     'Dorm','Hut','Other', 'Tent', 'Townhouse', 
                     'Treehouse', 'Villa','Lighthouse']
for i in propertytypetogroup:
    features.replace(to_replace=i, value='Others', inplace=True)
    
reviewscore=file_upsampled.loc[:,'Review Score Rating Bin']
reviewscore=data.loc[:,'Review Score Rating Bin']
from sklearn.preprocessing import LabelEncoder   
number=LabelEncoder()
features.iloc[:,1]=number.fit_transform(features.iloc[:,1])
features.iloc[:,2]=number.fit_transform(features.iloc[:,2])
features.iloc[:,3]=number.fit_transform(features.iloc[:,3])


X=features
y=reviewscore


#Prepare the data for training and testing
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#Logistic regression
X_train=sm.add_constant(X_train)
model=sm.MNLogit(y_train,X_train)
fit=model.fit()
from scipy import stats
stats.chisqprob = lambda chisq, df: stats.chi2.sf(chisq, df)
params=fit.params
y_train_pred=fit.predict(X_train)

#Naive Bayes classifier
#Train the model
from sklearn.naive_bayes import MultinomialNB
modelconfig = MultinomialNB(alpha=1.0, class_prior=None, fit_prior=True)
model=modelconfig.fit(X_train, y_train)
y_train_pred=model.predict(X_train)
from sklearn.metrics import accuracy_score
accuracy_score(y_train,y_train_pred)
#Evaluate the fit
from sklearn.metrics import confusion_matrix
confusionmatrix_train=confusion_matrix(y_train,y_train_pred)
plt.figure(figsize=(15*1.5,3*1.5))
plot_confusion_matrix(confusionmatrix_train, classes=['A','B','C'], 
            normalize=True, title='Normalized confusion matrix')
plt.show()

#Randomforest
from sklearn.ensemble import RandomForestClassifier
modelconfig=RandomForestClassifier(bootstrap=True, class_weight='balanced', criterion='entropy',
            max_depth=None, max_features='auto', max_leaf_nodes=None,
            min_impurity_decrease=0.0, min_impurity_split=None,
            min_samples_leaf=1, min_samples_split=2,
            min_weight_fraction_leaf=0.0, n_estimators=30, n_jobs=1,
            oob_score=False, random_state=0, verbose=0, warm_start=False)
model=modelconfig.fit(X_train,y_train)
y_train_pred=model.predict(X_train)
accuracy_score(y_train,y_train_pred)

#Evaluate the fit
from sklearn.metrics import confusion_matrix
confusionmatrix_train=confusion_matrix(y_train,y_train_pred)
plt.figure(figsize=(15*1.5,3*1.5))
plot_confusion_matrix(confusionmatrix_train, classes=['A','B','C'], 
            normalize=True, title='Normalized confusion matrix')
plt.show()

#Testing
y_test_pred=model.predict(X_test)
accuracy_score(y_test,y_test_pred)

#Evaluate the fit
from sklearn.metrics import confusion_matrix
confusionmatrix_test=confusion_matrix(y_test,y_test_pred)
plt.figure(figsize=(15*1.5,3*1.5))
plot_confusion_matrix(confusionmatrix_test, classes=['A','B','C'], 
            normalize=True, title='Normalized confusion matrix')
plt.show()

