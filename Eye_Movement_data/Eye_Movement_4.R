library(readxl)
library(ggplot2)
library(plotly)
# Setting plotly API key
Sys.setenv("plotly_username"="")
Sys.setenv("plotly_api_key"="")
# The code of uploading the plot: api_create(test, filename = "test")

# Professors' Feedback: 
# 1. 可以先畫pre-test跟post-test的scatter plot under different condition
# 2. 可以試試在每個scatter plot 中加入變數成為一個3D圖
# 3. 終極目標：當分群數設為5的時候，能剛好分為5個condition

# pre-test
pre_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_pretest.xlsx', range = "A1:U26")
pre_test$sum = apply(pre_test[, 2:21], 1, sum)
pre_test$ratio = pre_test$sum / 20


# post-test
post_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_posttest_score.xlsx')
# Create five post_test subsets under different condition
for (i in 1:5){
  assign(paste('post_test_Cond', i, sep = ''), subset(post_test, post_test$Condition == i, select = c(3, 1, 2, 4, 5)))
}


# 1. Visualize the data of Pre-test and Post-test. Could be the scatter plot of two data frames or bar plot of a specific one.

# Scatter Plot of Pre-test Score and Post-test Score Mean with full condition
par(mfrow = c(1, 1))
plot(pre_test$sum, aggregate(post_test, by = list(post_test$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Full Condition', xlim = c(0, 20), ylim = c(0, 3))
abline(line(pre_test$sum, aggregate(post_test, by = list(post_test$`Subject ID`), mean)$score), col = "red")
# Change the position of x and y
plot(aggregate(post_test, by = list(post_test$`Subject ID`), mean)$score, pre_test$sum,  
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Full Condition', xlim = c(0, 3), ylim = c(0, 20))
abline(line(aggregate(post_test, by = list(post_test$`Subject ID`), mean)$score, pre_test$sum), col = 'red')

# Scatter plot of Pre-test Score and Post-test Score Mean under different Condition
# I will use jitter() to add some noise since some of the dots are overlapping
par(mfrow = c(1, 5))
plot(jitter(pre_test$sum), aggregate(post_test_Cond1[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Under Condition 1', xlim = c(0, 20), ylim = c(0, 3))
plot(jitter(pre_test$sum), aggregate(post_test_Cond2[, ], by = list(post_test_Cond2$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Under Condition 2', xlim = c(0, 20), ylim = c(0, 3))
plot(jitter(pre_test$sum), aggregate(post_test_Cond3[, ], by = list(post_test_Cond3$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Under Condition 3', xlim = c(0, 20), ylim = c(0, 3))
plot(jitter(pre_test$sum), aggregate(post_test_Cond4[, ], by = list(post_test_Cond4$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Under Condition 4', xlim = c(0, 20), ylim = c(0, 3))
plot(jitter(pre_test$sum), aggregate(post_test_Cond5[, ], by = list(post_test_Cond5$`Subject ID`), mean)$score, 
     xlab = 'Pre-test Score', ylab = 'Post-test Score Mean', main = 'Under Condition 5', xlim = c(0, 20), ylim = c(0, 3))
# Change the position of x and y
par(mfrow = c(1, 5))
plot(jitter(aggregate(post_test_Cond1[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score), pre_test$sum, 
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Under Condition 1', xlim = c(0, 3), ylim = c(0, 20))
plot(jitter(aggregate(post_test_Cond2[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score), pre_test$sum, 
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Under Condition 2', xlim = c(0, 3), ylim = c(0, 20))
plot(jitter(aggregate(post_test_Cond3[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score), pre_test$sum, 
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Under Condition 3', xlim = c(0, 3), ylim = c(0, 20))
plot(jitter(aggregate(post_test_Cond4[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score), pre_test$sum, 
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Under Condition 4', xlim = c(0, 3), ylim = c(0, 20))
plot(jitter(aggregate(post_test_Cond5[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score), pre_test$sum, 
     xlab = 'Post-test Score Mean', ylab = 'Pre-test Score', main = 'Under Condition 5', xlim = c(0, 3), ylim = c(0, 20))

# 想法1: 在Full Condition的情況下，我們可以發現Pre-test Score和Post-test Score Mean兩者呈現正相關。
# 但是在不同condition中並沒有明顯正關係，這其實是一件好事，代表不同condition下的確存在差異。

# 想法2：回頭重新審視資料後，發現我可能錯了，我需要考慮到Full Condition和各別Condition在處理Post-test Score Mean上面的不同。
# 在Full Condition中，Post-test Score Mean要除以的分母為10，但是在各別Condition中的分母為2，
# 造成兩者在圖上的差異可能是因為上述原因，而不是每個Condition有自己的pattern。

# 想法3：突然想到，為什麼不將每個Condition合併為一個呢？與原本的Full Condition不同，其中的Post-test Score Mean的分母仍然是2。
par(mfrow = c(1, 1))
a = cbind(aggregate(post_test_Cond1[, ], by = list(post_test_Cond1$`Subject ID`), mean), pre_test_score = pre_test$sum)
b = cbind(aggregate(post_test_Cond2[, ], by = list(post_test_Cond1$`Subject ID`), mean), pre_test_score = pre_test$sum)
c = cbind(aggregate(post_test_Cond3[, ], by = list(post_test_Cond1$`Subject ID`), mean), pre_test_score = pre_test$sum)
d = cbind(aggregate(post_test_Cond4[, ], by = list(post_test_Cond1$`Subject ID`), mean), pre_test_score = pre_test$sum)
e = cbind(aggregate(post_test_Cond5[, ], by = list(post_test_Cond1$`Subject ID`), mean), pre_test_score = pre_test$sum)
combine_pre_post = rbind(a, b, c, d, e)
combine_pre_post$Condition = as.factor(combine_pre_post$Condition)  # Need to turn the type of the variable we need when coloring to factor.
plot(jitter(combine_pre_post$pre_test_score, factor = 0.5, amount = 0), jitter(combine_pre_post$score, factor = 0.5, amount = 0), 
     col = c('red', 'blue', 'green', 'brown', 'black')[combine_pre_post$Condition], xlab = 'Pre-test Score', 
     ylab = 'Post-test Score Mean', main = 'Combine All Condition', xlim = c(0, 20), ylim = c(0, 3), pch = 16,cex = 0.5)
legend(x="topright", legend = levels(combine_pre_post$Condition), col=c('red', 'blue', 'green', 'brown', 'black'), pch = 16, cex = 0.5)
# I use jitter() to add some noise since the dots are overlapping


# Barplot of Post-test Score Mean under different Condition
par(mfrow = c(5, 1))
barplot(table(factor(aggregate(post_test_Cond1[, ], by = list(post_test_Cond1$`Subject ID`), mean)$score, 
                     levels = seq(0, 3, 0.5))), xlab = 'Post-test Score Mean', main = 'Under Condition 1')
# text(x = a, y = dat$freqs, label = dat$freqs, pos = 3, cex = 0.8, col = "red")
barplot(table(factor(aggregate(post_test_Cond2[, ], by = list(post_test_Cond2$`Subject ID`), mean)$score, 
                     levels = seq(0, 3, 0.5))), xlab = 'Post-test Score Mean', main = 'Under Condition 2')
barplot(table(factor(aggregate(post_test_Cond3[, ], by = list(post_test_Cond3$`Subject ID`), mean)$score, 
                     levels = seq(0, 3, 0.5))), xlab = 'Post-test Score Mean', main = 'Under Condition 3')
barplot(table(factor(aggregate(post_test_Cond4[, ], by = list(post_test_Cond4$`Subject ID`), mean)$score, 
                     levels = seq(0, 3, 0.5))), xlab = 'Post-test Score Mean', main = 'Under Condition 4')
barplot(table(factor(aggregate(post_test_Cond5[, ], by = list(post_test_Cond5$`Subject ID`), mean)$score, 
                     levels = seq(0, 3, 0.5))), xlab = 'Post-test Score Mean', main = 'Under Condition 5')
#------------------------------------------------


# 2. Plot a 3D plot by adding new variables in the scatter plot of Pre-test score and Post-test mean score.
# I'll use the data frame I've created before in 'Eye_Movement_3.R'. If finding the xlsx file is lost, just create it again from the r file.
eye_movement_regressive = read.xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/eye_movement_regressive.xlsx')
eye_movement_skip = read.xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/eye_movement_skip.xlsx')
# Combine post_test, eye_movement_regressive, eye_movement_skip together.
df = cbind(post_test, regressive_times = eye_movement_regressive$regressive_times, skip_times = eye_movement_skip$skip_times)
df = cbind(df, pre_test_score = pre_test$sum)
colnames(df)[5] = 'post_test_score'
# Create five data frames under different conditions
for (i in 1:5){
  assign(paste('df_Cond', i, sep = ''), subset(df, df$Condition == i, select = c(3, 1, 2, 4, 5, 6, 7)))
}
# First aggregate the five data frames then combine the five data frames with Pre-test score
df_Cond1 = aggregate(df_Cond1[, ], by = list(df_Cond1$`Subject ID`), mean)
df_Cond2 = aggregate(df_Cond2[, ], by = list(df_Cond2$`Subject ID`), mean)
df_Cond3 = aggregate(df_Cond3[, ], by = list(df_Cond3$`Subject ID`), mean)
df_Cond4 = aggregate(df_Cond4[, ], by = list(df_Cond4$`Subject ID`), mean)
df_Cond5 = aggregate(df_Cond5[, ], by = list(df_Cond5$`Subject ID`), mean)
df_Cond1 = cbind(df_Cond1, pre_test_score = pre_test$sum)
df_Cond2 = cbind(df_Cond2, pre_test_score = pre_test$sum)
df_Cond3 = cbind(df_Cond3, pre_test_score = pre_test$sum)
df_Cond4 = cbind(df_Cond4, pre_test_score = pre_test$sum)
df_Cond5 = cbind(df_Cond5, pre_test_score = pre_test$sum)

# Draw 3D plot of Pre-test score, Post-test score mean, and regressive times under different conditions
plot_ly(df_Cond1, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 1',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))
plot_ly(df_Cond2, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 2',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))
plot_ly(df_Cond3, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 3',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))
plot_ly(df_Cond4, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 4',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))
plot_ly(df_Cond5, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 5',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))
# Draw a 3D plot with every condition within, notice that there's no denominator in post_test score
df$Condition = as.factor(df$Condition)
plot_ly(df, x = ~post_test_score, y = ~pre_test_score, z = ~regressive_times, color = ~Condition) %>%
  add_markers() %>%
  layout(title = 'Combine All Condition',
         scene = list(xaxis = list(title = 'Post-test Score'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Regressive Times')))


# Draw 3D plot of Pre-test score, Post-test score mean, and skip times under different conditions
plot_ly(df_Cond1, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 1',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond2, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 2',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond3, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 3',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond4, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 4',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond5, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 5',
         scene = list(xaxis = list(title = 'Post-test Score Mean'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))
# Draw a 3D plot with every condition within, notice that there's no denominator in post_test score
df$Condition = as.factor(df$Condition)
plot_ly(df, x = ~post_test_score, y = ~pre_test_score, z = ~skip_times, color = ~Condition) %>%
  add_markers() %>%
  layout(title = 'Combine All Condition',
         scene = list(xaxis = list(title = 'Post-test Score'), 
                      yaxis = list(title = 'Pre-test Score'), 
                      zaxis = list(title = 'Skip Times')))

# 想法1：從兩張combine plot會發現5種condition沒有明顯的分群，因此下一階段的任務應該是回到眼動儀資料去尋找新的變數。
# 除此之外，可以發現如果同時使用Pre-test Score以及Post-test Score，在3D分群上圖案會是片狀，因此我考慮在建構3D plot時，只放入兩者其一。

# 想法2：我嘗試取Pre-test Score, Regressive Times, Skip Times三個變數畫3D圖，可以發現Condition 2 相較於其他變數較為分的出來。
plot_ly(df_Cond1,x = ~pre_test_score, y = ~regressive_times, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 1',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond2,x = ~pre_test_score, y = ~regressive_times, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 2',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond3,x = ~pre_test_score, y = ~regressive_times, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 3',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond4,x = ~pre_test_score, y = ~regressive_times, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 4',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))
plot_ly(df_Cond5,x = ~pre_test_score, y = ~regressive_times, z = ~skip_times) %>%
  add_markers() %>%
  layout(title = 'Under Condition 5',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))
# Plot all condition together
plot_ly(df, x = ~pre_test_score, y = ~regressive_times, z = ~skip_times, color = ~Condition) %>%
  add_markers() %>%
  layout(title = 'Combine All Condition',
         scene = list(xaxis = list(title = 'Pre-test Score'), 
                      yaxis = list(title = 'Regressive Times'), 
                      zaxis = list(title = 'Skip Times')))

# 想法3：從想法2中我有一個新的idea，clustering一定要一次分好嗎，有沒有可能可以有步驟的層層剝離。原因是因為不同群體之間可能有差異的變數並不相同。
#----------------------------------------------------------------


# 3. Clustering
# Reference: https://rpubs.com/skydome20/R-Note9-Clustering
par(mfrow = c(1, 1))
test = df[, c(6, 7, 8)]
E.dist <- dist(test, method="euclidean")
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="Euclidean Distance")
cut.h.cluster <- cutree(h.E.cluster, k = 5)
table(cut.h.cluster, df$Condition)
cut.h.cluster 
table(cut.h.cluster)




kmeans.cluster <- kmeans(test, centers = 5) 
table(kmeans.cluster$cluster, df$Condition)  
df$Condition

table(kmeans.cluster$cluster)



