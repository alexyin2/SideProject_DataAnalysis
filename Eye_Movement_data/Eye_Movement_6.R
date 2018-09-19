library(readxl)
library(openxlsx)
library(plotly)
# Load data
# Pre-test
pre_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_pretest.xlsx', range = "A1:U26")
pre_test$sum = apply(pre_test[, 2:21], 1, sum)
pre_test$ratio = pre_test$sum / 20
# Post-test
post_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_posttest_score.xlsx')
# Regressive Times & Skip Times
# The code of creating the two data frames are in 'Eye_Movement_3.R'
eye_movement_regressive = read.xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/eye_movement_regressive.xlsx')
eye_movement_skip = read.xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/eye_movement_skip.xlsx')
# Time Spent Reading
time_spent = read.xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/time_spent.xlsx')

# Create our dataframe
# Combine post_test, eye_movement_regressive, eye_movement_skip, time_spent together.
df = cbind(post_test, regressive_times = eye_movement_regressive$regressive_times, 
           skip_times = eye_movement_skip$skip_times, time_spent = time_spent$time_spent)
df = cbind(df, pre_test_score = pre_test$sum)
colnames(df)[5] = 'post_test_score'