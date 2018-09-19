library(readxl)
# We're going to make a report that shows the time the testers have read in each article under different conditions.
# The demo is named "trial_info_summary_yc.xlsx".

#---------------------------------------
# The following code is just what we've done before.
trial_information = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = 1))
trial_information$id = 1
for (i in 2:25){
  temp = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = i))
  temp$id = i
  trial_information = rbind(trial_information, temp)
}

#---------------------------------------
# We'll write a function to help us get the trial_no and it's condition depending on the passage_no and id.
get_trial_no = function(id, passage_no){
  start = trial_information$trial_no[trial_information$id == id & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][1]
  end = trial_information$trial_no[trial_information$id == id & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][4]
  condition = trial_information$condition[trial_information$id == id & trial_information$passage_no == passage_no][1]
  return(c(start, end, condition))
}

# The trial_no we've find should be merged with the 'TRIAL_INDEX' in each tester's eye movement record.
# Besides, to know how much time the testers have spent reading in each page, we should look at the column 'CURRRNT_FIXED_END'
# By adding together the maximum number of 'CURRENT_FIXED_END' in each 'TRIAL_INDEX', we can get our answer.

#---------------------------------------
# We'll next write a loop to get the dataframe of all tester's eye movement record.
# Note that here we only extract the two columns that are needed.
eye_movement_record = NULL
for (i in 1:25){
  temp_1 = read_xlsx(paste('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/眼動軌跡解壓縮檔/', i, '.xlsx', 
                           sep = ''), range = cell_cols('N'))
  temp_2 = read_xlsx(paste('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/眼動軌跡解壓縮檔/', i, '.xlsx', 
                           sep = ''), range = cell_cols('GO'))
  temp_3 = cbind(temp_1, temp_2, id = i)
  eye_movement_record = rbind(eye_movement_record, temp_3)
}

#---------------------------------------
# Then, we will write a function to get the total time from tester's eye movement record.
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

#---------------------------------------
# Next, we'll start to construct our data frame.
trial_info_summary_1 = data.frame(row.names = append('words', sprintf("subj%s",seq(1:25)), 1))  # sprintf() helps to create a sequence of strings pretty easily.
trial_info_summary_1[sprintf("A%s", seq(1:20))] = NA  # Create a sequence of columns

for (i in c(1, 4, 5, 6, 8, 12, 14, 15, 16, 20)){
  trial_info_summary_1[1, i] = trial_information$words[trial_information$passage_no == i][1]
  for (j in 1: 25){
    temp = get_trial_no(j, i)
    trial_index_start = temp[1]
    trial_index_end = temp[2]
    condition = temp[3]
    total_time = get_time(j, trial_index_start, trial_index_end)
    trial_info_summary_1[j+1, i] = paste(condition, '_', total_time, sep = '')
  }
}

#---------------------------------------
# THe data frame is still not complete. Besides, we'll need to calculate every column's mean and standard deviation under different conditions.
# Note that there's an important problem that we need to solve.
# Since the class of the element in each column can only be the same, I decided to build another data frame and combine it with the previous one.
trial_info_summary_2 = data.frame(row.names = append(sprintf("mean(C%s)", seq(1:5)), sprintf("sd(C%s)", seq(1:5))))
trial_info_summary_2[sprintf("A%s", seq(1:20))] = 0  # Needs to be a numeric number instead of NA becuase NA is not calculatable.

for (i in c(1, 4, 5, 6, 8, 12, 14, 15, 16, 20)){
  for (k in 1:5){
    assign(paste("cond", k, sep = ""), c())
  }
  for (j in 1:25){
    temp = get_trial_no(j, i)
    trial_index_start = temp[1]
    trial_index_end = temp[2]
    condition = temp[3]
    total_time = get_time(j, trial_index_start, trial_index_end)
    if (condition == 1){
      cond1 = append(total_time, cond1)
    }
    else if (condition ==2){
      cond2 = append(total_time, cond2)
    }
    else if (condition == 3){
      cond3 = append(total_time, cond3)
    }
    else if (condition == 4){
      cond4 = append(total_time, cond4)
    }
    else {
      cond5 = append(total_time, cond5)
    }
  }
  trial_info_summary_2[c(1:5), i] = sapply(list(cond1, cond2, cond3, cond4, cond5), mean)
  trial_info_summary_2[c(6:10), i] = round(sapply(list(cond1, cond2, cond3, cond4, cond5), sd), 2)
}

#---------------------------------------------
# We can now combine 'trial_info_summary_1' and 'trial_info_summary_2'
# 'trial_info_summary' will be our final output.
trial_info_summary = rbind(trial_info_summary_1, trial_info_summary_2)
# I'll also make a connection of the time spent reading with the post-test data frame
post_test = read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng_posttest_score.xlsx')
time_spent = cbind(post_test, time_spent = 0)
for (i in 1:250){
  id = time_spent[i, 1]
  passage_no = time_spent[i, 2]
  temp = get_trial_no(id, passage_no)
  trial_index_start = temp[1]
  trial_index_end = temp[2]
  total_time = get_time(id, trial_index_start, trial_index_end)
  time_spent[i, 6] = total_time
}
# write.xlsx(time_spent, "/Users/imac-disc1/Desktop/Intern_尹崇安/眼動軌跡/time_spent.xlsx")
