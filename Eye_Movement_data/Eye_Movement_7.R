library(readxl)
library(openxlsx)
library(ggplot2)
library(plotly)
# Setting plotly API key
Sys.setenv("plotly_username"="")
Sys.setenv("plotly_api_key"="")
# The code of uploading the plot: api_create(test, filename = "test")

# Professor has offered me a task:
# Draw a plot showing the trajectory of the eye movement with time.
# 對於這個任務，我有兩個不同的想法：
# 想法1： X軸跟Y軸分別使用眼動儀資料中給的坐標，Z軸則使用時間，先將點標出來，不急著做連線。
# 想法2： 眼動儀中有提供一個變數：'CURRENT_FIX_NEAREST_INTEREST_AREA'，可以將這個變數作為X軸，Y軸則使用時間
# 我目前認為想法1、2的差別在於想法2可能在兩兩資料做對比時比較好觀察。
# 原因是想法1的X軸、Y軸放的是連續變數，而想法2雖然也是連續變數，但維度少且限定為整數。
# 更正：如果想法2想要做不同文章之間的對比，則X軸也需要經過scaling，無法維持整數。
# PS: There's some point that's quite important:
# 1. I should make sure that the plots are comparable with each other, meaning that the plot can fit in several different person's pattern.
# 2. There should be an option that if time should be scaled.
#-------------------------------------------------------------------


# Import data
eye_movement_record = NULL
for (i in 1:25){
  temp = read.xlsx(paste('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/眼動軌跡解壓縮檔/', i, '.xlsx', sep = ''), cols = c(14, 31, 43, 46, 197))
  temp = cbind(temp, id = i)
  eye_movement_record = rbind(eye_movement_record, temp)
}

trial_information = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = 1))
trial_information$id = 1
for (i in 2:25){
  temp = data.frame(read_xlsx('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/Eng-igaze_trial_info_revised.xlsx', sheet = i))
  temp$id = i
  trial_information = rbind(trial_information, temp)
}

# 1. Use x axis and y axis in eye_movement_record to draw 3D plot.

# Write the function that we need.
get_passage_trial_no = function(passage_no){  # Note that this function is different from the function: 'get_trial_no'
  get_passage_trial_no_df = data.frame(id = c(1:25), start = 0, end = 0, condition = 0, Article_Index = passage_no)
  for (i in 1:25){
    start = trial_information$trial_no[trial_information$id == i & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][1]
    end = trial_information$trial_no[trial_information$id == i & trial_information$passage_no == passage_no & trial_information$valid_trial == 1][4]
    condition = trial_information$condition[trial_information$id == i & trial_information$passage_no == passage_no][1]
    get_passage_trial_no_df[i, 2] = start
    get_passage_trial_no_df[i, 3] = end
    get_passage_trial_no_df[i, 4] = condition
  }
  return(get_passage_trial_no_df)
}

# Write the function of drawing the plot.
trajectory_3D_plot = function(Article_index, id = 0, group = 'condition', page = 1, time_scale = F){
  # Note that there's still a problem in this function.
  # When setting time_scale = TRUE, the 3D plot will be overflatted.
  # I guess it's because of the settings of plot_ly, but still need to figure out why.
  if (all(id == 0)){
    trajectory_plot_df = NULL
    get_passage_trial_no_df = get_passage_trial_no(Article_index)
    if (time_scale == TRUE){
      for (i in 1:25){
        temp = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + (page - 1), 
                      select=c(CURRENT_FIX_X, CURRENT_FIX_Y, CURRENT_FIX_END))
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        temp$CURRENT_FIX_END = lapply(temp$CURRENT_FIX_END, 
                                      function(x){(x - min(temp$CURRENT_FIX_END)) / (max(temp$CURRENT_FIX_END) - min(temp$CURRENT_FIX_END))})
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    else{
      for (i in 1:25){
        temp = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + (page - 1), 
                      select=c(CURRENT_FIX_X, CURRENT_FIX_Y, CURRENT_FIX_END))
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    trajectory_plot_df[, 'condition'] = as.factor(trajectory_plot_df[, 'condition'])
    trajectory_plot_df[, 'id'] = as.factor(trajectory_plot_df[, 'id'])
    if (group == 'id'){
      plot_3d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_X, y = ~CURRENT_FIX_Y, z = ~CURRENT_FIX_END, color = ~id) %>%
        add_markers() %>%
        layout(xaxis = list(range = c(0, 1500)), yaxis = list(range = c(0, 1500)), 
               title = paste('Article Index: ', Article_index, '; Page: ', page, '; Group by: ID', sep = ''),
               scene = list(xaxis = list(title = 'X axis'), 
                            yaxis = list(title = 'Y axis'), 
                            zaxis = list(title = 'Time')))
    }
    else{
      plot_3d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_X, y = ~CURRENT_FIX_Y, z = ~CURRENT_FIX_END, color = ~condition) %>%
        add_markers() %>%
        layout(xaxis = list(range = c(0, 1500)), yaxis = list(range = c(0, 1500)), 
               title = paste('Article Index: ', Article_index, '; Page: ', page, '; Group by: Condition', sep = ''),
               scene = list(xaxis = list(title = 'X axis'), 
                            yaxis = list(title = 'Y axis'), 
                            zaxis = list(title = 'Time')))
    }
  }
  
  else{
    trajectory_plot_df = NULL
    get_passage_trial_no_df = get_passage_trial_no(Article_index)
    if (time_scale == TRUE){
      for (i in id){
        temp = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + (page - 1), 
                      select=c(CURRENT_FIX_X, CURRENT_FIX_Y, CURRENT_FIX_END))
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        temp$CURRENT_FIX_END = lapply(temp$CURRENT_FIX_END, 
                                      function(x){(x - min(temp$CURRENT_FIX_END)) / (max(temp$CURRENT_FIX_END) - min(temp$CURRENT_FIX_END))})
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    else{
      for (i in id){
        temp = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + (page - 1), 
                      select=c(CURRENT_FIX_X, CURRENT_FIX_Y, CURRENT_FIX_END))
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    trajectory_plot_df[, 'condition'] = as.factor(trajectory_plot_df[, 'condition'])
    trajectory_plot_df[, 'id'] = as.factor(trajectory_plot_df[, 'id'])
    if (group == 'id'){
      plot_3d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_X, y = ~CURRENT_FIX_Y, z = ~CURRENT_FIX_END, color = ~id) %>%
        add_markers() %>%
        layout(xaxis = list(range = c(0, 1500)), yaxis = list(range = c(0, 1500)), 
               title = paste('Article Index: ', Article_index, '; Page: ', page, '; Group by: ID', sep = ''),
               scene = list(xaxis = list(title = 'X axis'), 
                            yaxis = list(title = 'Y axis'), 
                            zaxis = list(title = 'Time')))
    }
    else{
      plot_3d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_X, y = ~CURRENT_FIX_Y, z = ~CURRENT_FIX_END, color = ~condition) %>%
        add_markers() %>%
        layout(xaxis = list(range = c(0, 1500)), yaxis = list(range = c(0, 1500)), 
               title = paste('Article Index: ', Article_index, '; Page: ', page, '; Group by: Condition', sep = ''),
               scene = list(xaxis = list(title = 'X axis'), 
                            yaxis = list(title = 'Y axis'), 
                            zaxis = list(title = 'Time')))
    }
  }
  return(plot_3d)
}

# Show some examples:
trajectory_3D_plot(4, page = 2, time_scale = F)
trajectory_3D_plot(4, id = c(1, 3, 5, 7, 10), page = 2, group = 'id', time_scale = F)
#-----------------------------------------------------------


# 2. Use 'CURRENT_FIX_INTEREST_AREAS' and time to draw 2D plot
# We will use functions that were written previously in this file.
trajectory_2D_plot = function(Article_index, id = 0, group = 'condition', time_scale = F, area_scale = F){
  # Note that this function is still unfinished.
  # I haven't add the area_scale in the function. 
  if (all(id == 0)){
    trajectory_plot_df = NULL
    get_passage_trial_no_df = get_passage_trial_no(Article_index)
    if (time_scale == TRUE){
      for (i in 1:25){
        temp = NULL
        temp_1 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 1, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_1$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_2$CURRENT_FIX_END = temp_2$CURRENT_FIX_END + max(temp_1$CURRENT_FIX_END)
        temp_3 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 2, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_3$CURRENT_FIX_END = temp_3$CURRENT_FIX_END + max(temp_2$CURRENT_FIX_END)
        temp_4 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'end'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_4$CURRENT_FIX_END = temp_4$CURRENT_FIX_END + max(temp_3$CURRENT_FIX_END)
        temp = rbind(temp_1, temp_2, temp_3, temp_4)
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        temp$CURRENT_FIX_END = lapply(temp$CURRENT_FIX_END, 
                                      function(x){(x - min(temp$CURRENT_FIX_END)) / (max(temp$CURRENT_FIX_END) - min(temp$CURRENT_FIX_END))})
        if (area_scale == TRUE){
          temp$CURRENT_FIX_NEAREST_INTEREST_AREA = lapply(temp$CURRENT_FIX_NEAREST_INTEREST_AREA, 
                                                          function(x){(x - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA)) / (max(temp$CURRENT_FIX_NEAREST_INTEREST_AREA) - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA))})
        }
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    else{
      for (i in 1:25){
        temp = NULL
        temp_1 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 1, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_1$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_2$CURRENT_FIX_END = temp_2$CURRENT_FIX_END + max(temp_1$CURRENT_FIX_END)
        temp_3 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 2, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_3$CURRENT_FIX_END = temp_3$CURRENT_FIX_END + max(temp_2$CURRENT_FIX_END)
        temp_4 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'end'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_4$CURRENT_FIX_END = temp_4$CURRENT_FIX_END + max(temp_3$CURRENT_FIX_END)
        temp = rbind(temp_1, temp_2, temp_3, temp_4)
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        if (area_scale == TRUE){
          temp$CURRENT_FIX_NEAREST_INTEREST_AREA = lapply(temp$CURRENT_FIX_NEAREST_INTEREST_AREA, 
                                                          function(x){(x - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA)) / (max(temp$CURRENT_FIX_NEAREST_INTEREST_AREA) - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA))})
        }
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    trajectory_plot_df[, 'condition'] = as.factor(trajectory_plot_df[, 'condition'])
    trajectory_plot_df[, 'id'] = as.factor(trajectory_plot_df[, 'id'])
    if (group == 'id'){
      plot_2d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_NEAREST_INTEREST_AREA, y = ~CURRENT_FIX_END, color = ~id) %>%
        add_markers() %>%
        layout(title = paste('Article Index: ', Article_index, '; Group by ID', sep = ''),
               scene = list(xaxis = list(title = 'CURRENT FIX NEAREST INTEREST AREA'), 
                            yaxis = list(title = 'CURRENT FIX END')))
    }
    else{
      plot_2d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_NEAREST_INTEREST_AREA, y = ~CURRENT_FIX_END, color = ~condition) %>%
        add_markers() %>%
        layout(title = paste('Article Index: ', Article_index, '; Group by Condition', sep = ''),
               scene = list(xaxis = list(title = 'CURRENT FIX NEAREST INTEREST AREA'), 
                            yaxis = list(title = 'CURRENT FIX END')))
    }
  }
  
  else{
    trajectory_plot_df = NULL
    get_passage_trial_no_df = get_passage_trial_no(Article_index)
    if (time_scale == TRUE){
      for (i in id){
        temp = NULL
        temp_1 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 1, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_1$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_2$CURRENT_FIX_END = temp_2$CURRENT_FIX_END + max(temp_1$CURRENT_FIX_END)
        temp_3 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 2, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_3$CURRENT_FIX_END = temp_3$CURRENT_FIX_END + max(temp_2$CURRENT_FIX_END)
        temp_4 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'end'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_4$CURRENT_FIX_END = temp_4$CURRENT_FIX_END + max(temp_3$CURRENT_FIX_END)
        temp = rbind(temp_1, temp_2, temp_3, temp_4)
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        temp$CURRENT_FIX_END = lapply(temp$CURRENT_FIX_END, 
                                      function(x){(x - min(temp$CURRENT_FIX_END)) / (max(temp$CURRENT_FIX_END) - min(temp$CURRENT_FIX_END))})
        if (area_scale == TRUE){
          temp$CURRENT_FIX_NEAREST_INTEREST_AREA = lapply(temp$CURRENT_FIX_NEAREST_INTEREST_AREA, 
                                                          function(x){(x - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA)) / (max(temp$CURRENT_FIX_NEAREST_INTEREST_AREA) - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA))})
        }
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    else{
      for (i in id){
        temp = NULL
        temp_1 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 1, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_1$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_2$CURRENT_FIX_END = temp_2$CURRENT_FIX_END + max(temp_1$CURRENT_FIX_END)
        temp_3 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'start'] + 2, 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_2$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_3$CURRENT_FIX_END = temp_3$CURRENT_FIX_END + max(temp_2$CURRENT_FIX_END)
        temp_4 = subset(eye_movement_record, id == i & TRIAL_INDEX == get_passage_trial_no_df[i, 'end'], 
                        select=c(CURRENT_FIX_NEAREST_INTEREST_AREA, CURRENT_FIX_END))
        temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA = temp_4$CURRENT_FIX_NEAREST_INTEREST_AREA + max(temp_3$CURRENT_FIX_NEAREST_INTEREST_AREA)
        temp_4$CURRENT_FIX_END = temp_4$CURRENT_FIX_END + max(temp_3$CURRENT_FIX_END)
        temp = rbind(temp_1, temp_2, temp_3, temp_4)
        temp = cbind(id = i, temp, condition = get_passage_trial_no_df[i, 'condition'])
        if (area_scale == TRUE){
          temp$CURRENT_FIX_NEAREST_INTEREST_AREA = lapply(temp$CURRENT_FIX_NEAREST_INTEREST_AREA, 
                                                          function(x){(x - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA)) / (max(temp$CURRENT_FIX_NEAREST_INTEREST_AREA) - min(temp$CURRENT_FIX_NEAREST_INTEREST_AREA))})
        }
        trajectory_plot_df = rbind(trajectory_plot_df, temp)
      }
    }
    trajectory_plot_df[, 'condition'] = as.factor(trajectory_plot_df[, 'condition'])
    trajectory_plot_df[, 'id'] = as.factor(trajectory_plot_df[, 'id'])
    if (group == 'id'){
      plot_2d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_NEAREST_INTEREST_AREA, y = ~CURRENT_FIX_END, color = ~id) %>%
        add_markers() %>%
        layout(title = paste('Article Index: ', Article_index, '; Group by ID', sep = ''),
               scene = list(xaxis = list(title = 'CURRENT FIX NEAREST INTEREST AREA'), 
                            yaxis = list(title = 'CURRENT FIX END')))
    }
    else{
      plot_2d = plot_ly(trajectory_plot_df, x = ~CURRENT_FIX_NEAREST_INTEREST_AREA, y = ~CURRENT_FIX_END, color = ~condition) %>%
        add_markers() %>%
        layout(title = paste('Article Index: ', Article_index, '; Group by Condition', sep = ''),
               scene = list(xaxis = list(title = 'CURRENT FIX NEAREST INTEREST AREA'), 
                            yaxis = list(title = 'CURRENT FIX END')))
    }
  }
  return(plot_2d)
}

# Draw some samples
trajectory_2D_plot(1)
trajectory_2D_plot(1, group = 'id')
trajectory_2D_plot(1, id = c(1, 3, 4, 5), group = 'id')
trajectory_2D_plot(1, id = c(1, 3, 4, 5), group = 'id', area_scale = T)
trajectory_2D_plot(1, id = c(1, 3, 4, 5), group = 'id', area_scale = T, time_scale = T)






