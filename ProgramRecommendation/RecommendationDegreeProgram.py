import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# 1. Jaccard Similarity： we use this method to measure the similarity of different Programs
'''
Notice that there are still some problems remain unsolved:
1. The number of subject in each program are not the same, while some programs only have a few subjects, others may have 30 subjects.
2. I think the courses seperated in the file are too detailed, and some courses may share the same objective.
'''
df = pd.read_csv('RecommendationDegreeProgram/test1.csv', skiprows=1, header=None)
test = df.T
zeros = np.zeros((19, 19))
jac_sim = pd.DataFrame(zeros)
for i in range(19):
    for j in range(19):
        q = pd.DataFrame()
        q[0] = test[i]
        q[1] = test[j]
        q[2] = q[0] + q[1]
        q[3] = q[2] >= 1
        q[4] = q[2] == 2
        denominator = q[3].sum()
        molecular = q[4].sum()
        similarity = molecular / denominator
        jac_sim[i][j] = similarity

print(jac_sim)
# jac_sim.to_csv('jac_sim.csv', sep=',') Export to csv file
print(jac_sim[jac_sim > 0.3])
# After that, we've done some data process on the file and we read it in again
df1 = pd.read_csv('RecommendationDegreeProgram/jac_sim.csv', index_col=0)


# 2. Walk trap: we use this method to measure the similarity of students. Notice that it's not the real walk trap algorithm but it shares the same opinion.
# In this algorithm, we will have to set up some parameters: the steps of walk(When doing social network, usually 4 or 5 steps will be enough).
# We will also have to decide the weights of connecting each subject, which will mean the probability of choosing the subject to study.
# 
