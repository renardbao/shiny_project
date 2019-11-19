
time_sum <- function(time_char,time_char2 = NULL,sum_value){
  time_var <- as.POSIXct(time_char,format = '%H:%M:%S')
  if(is.null(time_char2)){
    time_var <- time_var + (sum_value*60)
    return(format(time_var,format = '%H:%M:%S')) 
  } else {
    time_var2 <- as.POSIXct(time_char2,format = '%H:%M:%S')
    time_var <- difftime(time_var,time_var2,units='mins') %>% 
                   as.numeric() %>% round()
    return(time_var)
    }
  
  
  
}

time_random <- function(time_char,seed = 1234){
  time_var <- as.POSIXct(time_char,format = '%H:%M:%S')
  time_len <- length(time_char)
  set.seed(seed)
  time_var <- time_var + runif(time_len,min = 0,max = 8*60*60)
  return(format(time_var,format = '%H:%M:%S')) 
  
}




data_to_json <- function(data) {
  jsonlite::toJSON(data, dataframe = "rows", auto_unbox = FALSE, rownames = TRUE)
}


#text_chart funciton
text_function <- function(input_data,unique_column){
  #整理資料製圖函數-----
  #這裡都可以不用動
  #資料欄位數
  
  col_num <- input_data %>% names %>% length()
  #unique_column 在data第幾欄
  col_where <- which(names(input_data) %in% unique_column)
  #change data factor type th char
  require(data.table)
  input_data  %<>% as.data.table()
  
  #檢查是否為factor 是的話轉為char
  checkandchange <- function(d){
    if(is.factor(d)){
      as.character(d)
    }else{
      d
    }
  }
  #用datatable的set來針對
  for(j in names(input_data)) set(input_data, j = j,value = checkandchange(input_data[[j]]))
  input_data %<>% as.data.frame()
  unique_row <- input_data[which(input_data[,unique_column] > 1),]
  
  add_data <- NULL
  if(nrow(unique_row) != 0 ){
    #根據有多少value大於1執行次數
    for(i in 1:nrow(unique_row)){
      #根據value新增value-1的資料進去
      for(m in 1:(unique_row[,unique_column]-1)){
        #根據欄位生成要添加的資料
        add_data <- NULL
        for(n in 1: col_num){
          add_data <- c(add_data,unique_row[i,n])
        }
        add_data[col_where] <- m
        
        input_data <- rbind(input_data,add_data)
        
      }
    }
    return(input_data)
  }
  return(input_data)
}

