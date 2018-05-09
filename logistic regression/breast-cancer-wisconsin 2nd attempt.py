import pandas as pd
import statsmodels.api as sm
import numpy as np
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix
import itertools
#Preprocessing steps, selecting and calculating necessary columns and transform the data
df = pd.read_csv('C:\\Users\\DELL\\Google Drive\\JVN couse materials\\Projects\\Practice projects\\logistic regression\\breast-cancer-wisconsin.csv')
    #explore the data
df.describe()
df.std()
tempt = pd.crosstab(df['ClumpThickness'], df['Class'])
df.hist()
plt.show()
    #prepare the data
df['Class'].replace('benign',0, inplace=True)
df['Class'].replace('malignant',1, inplace=True)
df.replace('?',10**9,inplace=True)
df.isnull().values.any()#this gives false, data has no missing value
X = df.drop(['Class','Code'], 1)
X = pd.DataFrame({"Constant":np.ones(len(X))}).join(pd.DataFrame(X))
y = df['Class']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

X_train = X_train.astype(int)
y_train = y_train.astype(int)
X_train = np.array(X_train)
y_train = np.array(y_train)
X_test = X_test.astype(int)
y_test = y_test.astype(int)
X_test = np.array(X_test)
y_test = np.array(y_test)
#Train 
clf = sm.Logit(y_train, X_train)
model = clf.fit()
trainlabel = clf.endog
betahat = model.params

#evaluation
trainscore = clf.score(model.params) #error is small
model.summary()
comparetrain = pd.DataFrame({'y train': y_train, 'train label': trainlabel}) #train shows pretty good results

predictionfortest = model.predict(X_test)
predictlabel = []
for i in predictionfortest:
    if i >= 0.5:
        predictlabel.append(1)
    else: predictlabel.append(0)

testscore = ((predictlabel-y_test)**2).mean()
comparetest = pd.DataFrame({'y test': y_test, 'predicted label': predictlabel})


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
# Compute confusion matrix
cnf_matrix = confusion_matrix(y_test, predictlabel)
np.set_printoptions(precision=2)


# Plot non-normalized confusion matrix
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=['0','1'], title='Confusion matrix, without normalization')

# Plot normalized confusion matrix
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=['0','1'], normalize=True, title='Normalized confusion matrix')

plt.show()