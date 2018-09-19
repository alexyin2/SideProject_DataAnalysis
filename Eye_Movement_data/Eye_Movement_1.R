library(readxl)
trial_information = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = 1))
trial_information$id = 1
for (i in 2:25){
  temp = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = i))
  temp$id = i
  trial_information = rbind(trial_information, temp)
}

# Now, we've get a dataframe that combines all testers' data
# -------------------------
# Next, we would make a dataframe for each testers, allowing us to connect the testers' data with the eye movement data later.
for (i in 1:25){
  demo = data.frame(row.names = c('text1', 'text2'), 'Cond1' = c(0, 0), 'Cond2' = c(0, 0), 'Cond3' = c(0, 0), 'Cond4' = c(0, 0), 'Cond5' = c(0, 0))
  for (j in 1:2){
    for (k in 1:5){
      first = trial_information$trial_no[trial_information$id == i & trial_information$text == j & trial_information$condition == k & trial_information$valid_trial == 1][1]
      last = trial_information$trial_no[trial_information$id == i & trial_information$text == j & trial_information$condition == k & trial_information$valid_trial == 1][4]
      element = paste(first, "_", last)  # paste() allows us to combine varialbes and strings together to be a new string.
      demo[j, k] = element
    }
  }
  assign(paste("trial_info_", i, sep = ""), demo)  # assign() allows us to name a variable.
}

# ----------------------------------
# There is a problem in the above output that the varialbes we've created are not easy to see, and we will have to click them one by one.
# As a result, I will try to turn it into a big data frame in the following code.
new = data.frame(NULL)
for (i in 1:25){
  demo = data.frame('1' = c(paste("trial_info_", i, sep = ""), 'text1', 'text2'), '2' = c('Cond1', 0, 0), '3' = c('Cond2', 0, 0), '4' = c('Cond3', 0, 0), 
                    '5' = c('Cond4', 0, 0), '6' = c('Cond5', 0, 0), stringsAsFactors = FALSE)
  # Note that adding stringAsFactors = FALSE is pretty important as I spent a lot of time to find this solution. One can try to cancel it and see what will happen.
  for (j in 1:2){
    for (k in 1:5){
      first = trial_information$trial_no[trial_information$id == i & trial_information$text == j & trial_information$condition == k & trial_information$valid_trial == 1][1]
      last = trial_information$trial_no[trial_information$id == i & trial_information$text == j & trial_information$condition == k & trial_information$valid_trial == 1][4]
      element = paste(first, "_", last)  # paste() allows us to combine varialbes and strings together to be a new string.
      demo[j+1, k+1] = element
    }
  }
  new = rbind(new, demo)
  new[nrow(new)+1, ] = NA
}
# Now we can show others the whole data by just printing out the 'new' data frame.
