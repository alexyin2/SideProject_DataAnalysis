library(readxl)
library(openxlsx)
library(plotly)
# 終極目標：當分群數設為5的時候，能剛好分為5個condition
# 當下階段的任務應該是回到眼動儀資料去尋找新的變數，
# 而為了尋找新的變數去做transformation，我對於資料的熟悉程度還不夠，應該需要做一次徹底的data exploratory
# 能畫圖的畫圖，能畫distribution的畫distribution，需要grouping後一起看的做grouping。
eye_movement_record = NULL
for (i in 1:25){
  # The columns I've selected are 'CURRENT_FIX_DURATION', 'CURRENT_FIX_END', 'CURRENT_FIX_INTEREST_AREA_INDEX', and 'TRIAL_INDEX'.
  temp = read.xlsx(paste('/Users/alex/Desktop/Intern_尹崇安/眼動軌跡/眼動軌跡解壓縮檔/', i, '.xlsx', sep = ''), cols = c(13, 14, 21, 197))
  temp = cbind(temp, id = i)
  eye_movement_record = rbind(eye_movement_record, temp)
}


