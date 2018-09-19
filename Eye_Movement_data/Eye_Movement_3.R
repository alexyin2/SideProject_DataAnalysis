library(readxl)
library(openxlsx)
library(ggplot2)
library(foreign)
library(nnet)
# In this file, My goal is try to make a connection between pre-test, eye movement record, and the post-test.
# 一些post_test想法：簡單的文本群跟困難的文本群，各自比較在不同Condition下是否有顯著差異。
# 一些eye movement record想法：(1)依據眼動軌跡將testers分群後再分析？(2)Will the number of skipping words be more or less under different conditions?
# (3) Will the number of regressives be more or less under different conditions?  可以針對不同文章去做比對，理由是文本內容或許也是影響的一大因素。
#-------------------------------------
# I'll first try to do some summary of the three different data.

#-------------------------------------
# pre-test summary
# pre-test shows how the testers perform in their english ability test.
pre_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_pretest.xlsx', range = "A1:U26")
pre_test$sum = apply(pre_test[, 2:21], 1, sum)
pre_test$ratio = pre_test$sum / 20
pre_test = pre_test[order(pre_test$ratio, decreasing = T), ]
# We can see that the tester 006 performs pretty well when tester 010 performs badly. 
# This may be a question we need to take into account.

#-------------------------------------
# post-test summary
# After reading each article, the testers will be asked 3 questions about the article.
# The column 'subjective rating' means that the tester's will be required to rate the difficulty of the article they've read.
post_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_posttest_score.xlsx')

# I will like to create a data frame that describes the answer outcome under different conditions for each article.
# First we'll create a function that helps us get the information from the 'post_test'
get_information = function(Article_index, Condition){
  score = post_test$score[post_test$`Article index` == Article_index & post_test$Condition == Condition]
  subjective_rating = post_test$subjective_rating[post_test$`Article index` == Article_index & post_test$Condition == Condition]
  return(list(score, subjective_rating))
}

# Then we'll create our data frame.
post_test_summary = data.frame('Article_index' = rep(sprintf('Article_%s', c(1, 4, 5, 6, 8, 12, 14, 15, 16, 20)), each = 5), 
                               Condition = rep(1:5, 10), stringsAsFactors = FALSE)
post_test_summary[c('Score_mean', 'subjective_rating_mean', 'Score_sd', 'subjective_rating_sd')] = 0

for (i in 1:10){
  Article_index = c(1, 4, 5, 6, 8, 12, 14, 15, 16, 20)
  for (j in 1:5){
    ## post_test_summary[(i - 1)*5 + j, 1] = paste(post_test_summary[(i - 1)*5 + j, 1], '_', j, sep = '')
    post_test_summary[(i - 1)*5 + j, 3] = mean(unlist(get_information(Article_index[i], j)[1]))
    post_test_summary[(i - 1)*5 + j, 4] = mean(unlist(get_information(Article_index[i], j)[2]))
    post_test_summary[(i - 1)*5 + j, 5] = sd(unlist(get_information(Article_index[i], j)[1]))
    post_test_summary[(i - 1)*5 + j, 6] = sd(unlist(get_information(Article_index[i], j)[2]))
  }
}
## ggplot(post_test_summary, aes(x = reorder(post_test_summary$Article_index, rep(c(1, 4, 5, 6, 8, 12, 14, 15, 16, 20), each = 5)), y = Score_mean, fill = Condition)) + geom_bar(stat = "identity")
# Unfortunately, I've met a problem that my plot looks awful and may require more time to fix it.
# write.xlsx(post_test_summary, "/Users/imac-disc1/Desktop/Intern_尹崇安/眼動軌跡/post_test_summary.xlsx")

# The following sorting may also worth for more detailed research.
post_test_summary[with(post_test_summary, order(Condition, Article_index)), ]

#-------------------------------------
# eye movement record summary
eye_movement_record = NULL
for (i in 1:25){
  # The columns I've selected are 'CURRENT_FIX_DURATION', 'CURRENT_FIX_END', 'CURRENT_FIX_INTEREST_AREA_INDEX', and 'TRIAL_INDEX'.
  temp = read.xlsx(paste('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/眼動軌跡解壓縮檔/', i, '.xlsx', sep = ''), cols = c(13, 14, 21, 197))
  temp = cbind(temp, id = i)
  eye_movement_record = rbind(eye_movement_record, temp)
}


# 1. Case : Number of Skipping Words
# Now I decided to look at the number of skipping words in each testers to see how many words they have skipped.
# I would make a connection between the number of skipping words and the post_test.
trial_information = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = 1))
trial_information$id = 1
for (i in 2:25){
  temp = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = i))
  temp$id = i
  trial_information = rbind(trial_information, temp)
}

get_trial_no = function(id, passage_no){
  start = trial_information$trial_no[trial_information$id == id & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][1]
  end = trial_information$trial_no[trial_information$id == id & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][4]
  condition = trial_information$condition[trial_information$id == id & trial_information$passage_no == passage_no][1]
  return(c(start, end, condition))
}

# The above are some functions that we've written before in 'Eye_Movement_2.R'
# Next we'll start our research.
# I'll first write a function that scans the number of incidents when the testers' skipped more than 'n' words.
skip_scanning = function(input_vector, skip_words){
  temp = c()
  a = 0
  for (i in 1:length(input_vector)){
    if (is.na(input_vector[i])){
      next
    }
    
    else{
      if(input_vector[i] %in% temp == FALSE){
        temp = append(temp, input_vector[i])
        
        if (length(temp) >= 2){
          if (input_vector[i] - max(temp[1:length(temp)-1]) >= skip_words){
            a = a + 1
          }
        }
      }
    }
  }
  return(a)
}

# Finally, we'll start to build our data frame.
eye_movement_skip = cbind(post_test, skip_times = 0)
for (i in 1:250){
  start = get_trial_no(eye_movement_skip[i, 1], eye_movement_skip[i, 2])[1]
  end = get_trial_no(eye_movement_skip[i, 1], eye_movement_skip[i, 2])[2]
  input_vector_1 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_skip[i, 1] & 
                                                                     eye_movement_record$TRIAL_INDEX == start])
  input_vector_2 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_skip[i, 1] & 
                                                                       eye_movement_record$TRIAL_INDEX == start + 1])
  input_vector_3 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_skip[i, 1] & 
                                                                       eye_movement_record$TRIAL_INDEX == start + 2])
  input_vector_4 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_skip[i, 1] & 
                                                                       eye_movement_record$TRIAL_INDEX == end])
  eye_movement_skip[i, 6] = skip_scanning(input_vector_1, 2) + skip_scanning(input_vector_2, 2) + skip_scanning(input_vector_3, 2) + skip_scanning(input_vector_4, 2)
}

# Analysis
aggregate(eye_movement_skip[, c(3, 4, 5, 6)], by = list(eye_movement_skip$Condition), mean)
aggregate(eye_movement_skip[, c(3, 4, 5, 6)], by = list(eye_movement_skip$Condition), sd)

# Here we split 'eye_movement_summary' into two data frames depending on the difficulty of the article. (text1: easy; text2: hard)
eye_movement_skip_text1 = eye_movement_skip[seq(1, 250, by = 2), ]
eye_movement_skip_text2 = eye_movement_skip[-seq(1, 250, by = 2), ]
aggregate(eye_movement_skip_text1[, c(3, 4, 5, 6)], by = list(eye_movement_skip_text1$Condition), mean)
aggregate(eye_movement_skip_text2[, c(3, 4, 5, 6)], by = list(eye_movement_skip_text2$Condition), mean)
aggregate(eye_movement_skip_text1[, c(3, 4, 5, 6)], by = list(eye_movement_skip_text1$Condition), sd)
aggregate(eye_movement_skip_text2[, c(3, 4, 5, 6)], by = list(eye_movement_skip_text2$Condition), sd)

cor(eye_movement_skip$score, eye_movement_skip$skip_times)
cor(eye_movement_skip_text1$score, eye_movement_skip_text1$skip_times)
cor(eye_movement_skip_text2$score, eye_movement_skip_text2$skip_times)

cor(eye_movement_skip$subjective_rating, eye_movement_skip$skip_times)
cor(eye_movement_skip_text1$subjective_rating, eye_movement_skip_text1$skip_times)
cor(eye_movement_skip_text2$subjective_rating, eye_movement_skip_text2$skip_times)



# 2. Case: Number of Regressives
# Some functions which are needed in this case ('df', and 'get_trial_no') are in Case 1.
# I'll first write a function that scans the number of regressives when the testers' went back to previous words.
regressive_scanning = function(input_vector){
  temp = c()
  a = 0
  for (i in 1:length(input_vector)){
    if (is.na(input_vector[i])){
      next
    }
    
    else{
      temp = append(temp, input_vector[i])
      if (length(temp) >= 2){
        if (input_vector[i] - temp[length(temp)-1] < 0){
          a = a + 1
        }
      }
    }
  }
  return(a)
}

# We'll then build our data frame.
eye_movement_regressive = cbind(post_test, regressive_times = 0)
for (i in 1:250){
  start = get_trial_no(eye_movement_regressive[i, 1], eye_movement_regressive[i, 2])[1]
  end = get_trial_no(eye_movement_regressive[i, 1], eye_movement_regressive[i, 2])[2]
  input_vector_1 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_regressive[i, 1] & 
                                                                                    eye_movement_record$TRIAL_INDEX == start])
  input_vector_2 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_regressive[i, 1] & 
                                                                                    eye_movement_record$TRIAL_INDEX == start + 1])
  input_vector_3 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_regressive[i, 1] & 
                                                                                    eye_movement_record$TRIAL_INDEX == start + 2])
  input_vector_4 = as.numeric(eye_movement_record$CURRENT_FIX_INTEREST_AREA_INDEX[eye_movement_record$id == eye_movement_regressive[i, 1] & 
                                                                                    eye_movement_record$TRIAL_INDEX == end])
  eye_movement_regressive[i, 6] = regressive_scanning(input_vector_1) + regressive_scanning(input_vector_2) + 
    regressive_scanning(input_vector_3) + regressive_scanning(input_vector_4)
}

# Analysis
aggregate(eye_movement_regressive[, c(3, 4, 5, 6)], by = list(eye_movement_regressive$Condition), mean)
aggregate(eye_movement_regressive[, c(3, 4, 5, 6)], by = list(eye_movement_regressive$Condition), sd)

# Here we split 'eye_movement_summary' into two data frames depending on the difficulty of the article. (text1: easy; text2: hard)
eye_movement_regressive_text1 = eye_movement_regressive[seq(1, 250, by = 2), ]
eye_movement_regressive_text2 = eye_movement_regressive[-seq(1, 250, by = 2), ]
aggregate(eye_movement_regressive_text1[, c(3, 4, 5, 6)], by = list(eye_movement_regressive_text1$Condition), mean)
aggregate(eye_movement_regressive_text2[, c(3, 4, 5, 6)], by = list(eye_movement_regressive_text2$Condition), mean)
aggregate(eye_movement_regressive_text1[, c(3, 4, 5, 6)], by = list(eye_movement_regressive_text1$Condition), sd)
aggregate(eye_movement_regressive_text2[, c(3, 4, 5, 6)], by = list(eye_movement_regressive_text2$Condition), sd)

cor(eye_movement_regressive$score, eye_movement_regressive$regressive_times)
cor(eye_movement_regressive_text1$score, eye_movement_regressive_text1$regressive_times)
cor(eye_movement_regressive_text2$score, eye_movement_regressive_text2$regressive_times)

cor(eye_movement_regressive$subjective_rating, eye_movement_regressive$regressive_times)
cor(eye_movement_regressive_text1$subjective_rating, eye_movement_regressive_text1$regressive_times)
cor(eye_movement_regressive_text2$subjective_rating, eye_movement_regressive_text2$regressive_times)


# 3. Case 3: Logistic Regression
# I'll use the data created above: 'eye_movement_skip', and 'eye_movement_regressive'.
logistic_data = cbind(eye_movement_skip, regressive_times = eye_movement_regressive[, 6])

# Besides, I'll also try to add more columns: 'text_difficulty' and 'spent_time'.
# I'll first add a column 'text_difficulty' in our data frame. (0 means easy; 1 means hard)
logistic_data$text_difficulty = rep(c(0, 1), 125)

# Next, the function below helps to find the time each tester's spent in reading the article.
get_time = function(id, trial_index_start, trial_index_end){
  first = max(eye_movement_record$CURRENT_FIX_END[eye_movement_record$id == id & 
                                                    eye_movement_record$TRIAL_INDEX == trial_index_start])
  second = max(eye_movement_record$CURRENT_FIX_END[eye_movement_record$id == id & 
                                                     eye_movement_record$TRIAL_INDEX == trial_index_start + 1])
  third = max(eye_movement_record$CURRENT_FIX_END[eye_movement_record$id == id & 
                                                    eye_movement_record$TRIAL_INDEX == trial_index_start + 2])
  fourth = max(eye_movement_record$CURRENT_FIX_END[eye_movement_record$id == id & 
                                                     eye_movement_record$TRIAL_INDEX == trial_index_end])
  return(first + second + third + fourth)
}

logistic_data$time = 0
for (i in 1:250){
  start = get_trial_no(logistic_data[i, 1], logistic_data[i, 2])[1]
  end = get_trial_no(logistic_data[i, 1], logistic_data[i, 2])[2]
  logistic_data[i, 'time'] = get_time(logistic_data[i, 1], start, end)
}
# Now we're done with our data frame and we can start running the logistic regression.
logistic_data[, c(4, 6, 7, 9)] = scale(logistic_data[, c(4, 6, 7, 9)]) # Normalization
logistic_data$Condition = factor(logistic_data$Condition)  # turn Condition into a factor
logistic_data$text_difficulty = factor(logistic_data$text_difficulty) # turn text_difficulty into a factor
logistic_data$score = factor(logistic_data$score)  # turn score into a factor
logistic_data$score = relevel(logistic_data$score, ref = '0')  # set the standard as score '0'
model = multinom(score ~ Condition+subjective_rating+skip_times+regressive_times+text_difficulty+time, data = logistic_data)
summary(model)
z = summary(model)$coefficients/summary(model)$standard.errors
p = (1 - pnorm(abs(z), 0, 1)) * 2
p

#--------------------------------------
# Sadly to say, the research I've done didn't show some significant patterns or hidden variables.
# The reason may be that I'm too hury. I didn't really understand the trend or patterns of the variables.
# I must get back to the start, and try to see some patterns by drawing plots or graphs.

