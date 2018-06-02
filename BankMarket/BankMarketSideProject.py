# Resource: https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import cross_val_score
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
# Load data
df = pd.read_csv('bank-full.csv', sep=';')
df.head()
# Summary Statistics and plots
df.describe(include='all')
df.loc[:, ['age', 'day']].boxplot()
df.loc[:, ['balance']].boxplot()
df.loc[:, ['duration']].boxplot()
df.y.value_counts()
# Data Pre-processing
X = df.iloc[:, :-1]
i = ['job', 'marital', 'education', 'default', 'housing', 'loan', 'contact', 'month', 'poutcome']
for a in i:
    X[a] = X[a].astype('category').cat.codes  # turn categorical values into integers

X = X.as_matrix()
Y = df.iloc[:, -1]
Y = Y.replace({'no': 0, 'yes': 1})  # turn yes, no into 1, 0
Y = Y.as_matrix()
# Apply different algorithms
# 1. KNN
knn = KNeighborsClassifier(n_neighbors=5, weights='distance', n_jobs=-1)
knn.fit(X, Y)
print(knn.score(X, Y))
knn_scores = cross_val_score(knn, X, Y, cv=5, scoring='accuracy')
print(knn_scores)

# 2. Decision Tree
tree = DecisionTreeClassifier(criterion='entropy', max_depth=50)
tree.fit(X, Y)
print(tree.score(X, Y))
tree_scores = cross_val_score(tree, X, Y, cv=5, scoring='accuracy')
print(tree_scores)
# Plot to see which performs better
plt.plot(knn_scores, label='KNN')
plt.plot(tree_scores, label='Decision Tree')
plt.legend()
plt.show()
