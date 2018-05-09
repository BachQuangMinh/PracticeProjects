import pandas as pd
import statsmodels.api as sm
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
import itertools
from scipy import stats
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score

#confusion matrix function
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

#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Desktop\\mini-hackathon II-final round\\mini-hackathon II-final\\train.csv')
#explore the data
df.default.describe()
df.std()

#correlation
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

#prepare the data
df=df[['Gross Profit','Revenue','Pretax Income','Total Assets','Total Liabilities','Total Current Assets','Total Current Liabilities','Inventories','Interest Expense','Short-Term Borrowings','Cash & Near Cash Items','Accounts & Notes Receivable','default']]
df=df.drop([169],0)

X=df[['Gross Profit','Revenue','Pretax Income','Total Assets','Total Liabilities','Total Current Assets','Total Current Liabilities','Inventories','Interest Expense','Short-Term Borrowings','Cash & Near Cash Items']]
y=df['default']

#Split the data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#Train 
clf_0 = LogisticRegression(class_weight='balanced').fit(X_train, y_train)
pred_train = clf_0.predict(X_train)

# How's the accuracy?
print(accuracy_score(pred_train, y_train))

#summary
params = np.append(clf_0.intercept_,clf_0.coef_)
predictions = pred_train
ones=np.ones(len(X_train))
newX=X_train
newX['constant']=ones

MSE = (sum((y_train-predictions)**2))/(len(newX)-len(newX.columns))

# Note if you don't want to use a DataFrame replace the two lines above with
# newX = np.append(np.ones((len(X_train),1)), X, axis=1)
# MSE = (sum((y_train-predictions)**2))/(len(newX)-len(newX[0]))

var_b = MSE*(np.linalg.inv(np.dot(newX.T,newX)).diagonal())
sd_b = np.sqrt(var_b)
ts_b = params/ sd_b
p_values =[2*(1-stats.t.cdf(np.abs(i),(len(newX)-1))) for i in ts_b]

myDF3 = pd.DataFrame()
myDF3["Coefficients"],myDF3["Standard Errors"],myDF3["t values"],myDF3["Probabilites"] = [params,sd_b,ts_b,p_values]
print(myDF3)

#evaluation
pred_test = clf_0.predict(X_test)

# Compute confusion matrix for train
cnf_matrix = confusion_matrix(y_train, pred_train)
np.set_printoptions(precision=2)
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=['0','1'], normalize=True, title='Normalized confusion matrix')

# Compute confusion matrix for test
cnf_matrix = confusion_matrix(y_test, pred_test)
np.set_printoptions(precision=2)
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=['0','1'], normalize=True, title='Normalized confusion matrix')
