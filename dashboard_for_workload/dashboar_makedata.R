
library(magrittr)
library(dplyr)
library(ggplot2)
library(DT)
library(gridExtra)
library(reshape2)
library(stringr)
#模擬專案案件資料
name <- paste0("專案",1:13)
time <- c("08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00") 
#製造案件量不同分布評級
done_type_p <- data.frame(per = c(1,1,1,1,1,1,1,0.7,0.5,0.3),
                          good = c(1,1,1,1,1,1,0.7,0.5,0.3,0.15),
                          so = c(1,1,1,1,1,0.9,0.7,0.5,0.3,0.1),
                          bad = c(1,1,1,1,0.8,0.65,0.5,0.3,0.15,0.05),
                          poor = c(1,1,1,0.8,0.7,0.5,0.35,0.15,0.1,0.05),
                          row.names = time,
                          stringsAsFactors = F
                          )
done_type <- data.frame(type = c("per","per","per","bad","bad","good","good","good","good","so","so","so","poor"),
                        row.names = name,
                        stringsAsFactors = F)
#人力預估(案件預估消耗量)
case_pre <- data.frame(time,預估消耗量 = c(rep(40,5),rep(35,5)))
#模擬案件
set.seed(1001)
data_test <- data.frame(stringsAsFactors = F,
                        type = sample(name,5200,replace = T),
                        get_time = sample(time,5200,replace = T),
                        date = as.Date("2019-01-21"))
#根據評級模擬案件完成
set.seed(1234)
data_test$done <- NA
for(i in 1 : nrow(data_test))
{
  data_test[i,"done"] <- sample(c("Y","N"),1,
                                prob =c(done_type_p[data_test[i,"get_time"],done_type[data_test[i,"type"],]],
                                        1-done_type_p[data_test[i,"get_time"],done_type[data_test[i,"type"],]]))
  
}

#讓專案1 14:00~15:00多一點
data_test <- rbind(data_test,
                   data.frame(type = rep("專案1",15),
                              get_time = rep("14:00",15),
                              done = rep("Y",15),
                              date = as.Date("2019-01-21")
                   ))
data_test <- rbind(data_test,
                   data.frame(type = rep("專案1",5),
                              get_time = rep("15:00",5),
                              done = rep("Y",5),
                              date = as.Date("2019-01-21")
                   ))
#增加一年完成資料
data_test <- rbind(data_test,
                   data.frame(type = sample(name,1200000,replace = T),
                              get_time = sample(time,1200000,replace = T),
                              done = "Y",
                              date = as.Date(1:384,"2018-01-01") %>% sample(1200000,replace = T)
                   ))
#增加兩個月份未做完資料
set.seed(1234)
data_test <- rbind(data_test,
                   data.frame(type = sample(name,12000,replace = T),
                              get_time = sample(time,12000,replace = T),
                              done = "N",
                              date = as.Date(60:1,"2018-11-21") %>% 
                                     sample(12000,replace = T,prob = seq(from = 0.03,to = 0.004,length.out = 60 ))
                   ))
#新增詳細進件時間----
set.seed(1234)
#新增進件分鐘
data_test$get_time_minute <- floor(rnorm(nrow(data_test),0.3,0.1)*100) %>% as.character()
set.seed(1234)
data_test$get_time_minute <- ifelse(nchar(data_test$get_time_minute) == 1,
                                    paste0("0",data_test$get_time_minute),
                                    data_test$get_time_minute)
#新增進件小時
#numeric
data_test$get_time_hour <-  str_sub(data_test$get_time,1,2) %>% as.numeric()
#詳細進件時間
data_test$get_time2 <- paste0(data_test$get_time_hour,":",data_test$get_time_minute) 
data_test$get_time2 <- ifelse(nchar(data_test$get_time2) == 4,
                              paste0("0",data_test$get_time2),
                              data_test$get_time2)
#作業時間----
set.seed(1234)
data_test$work_runtime <- floor(rnorm(nrow(data_test),0.2,0.01)*100)

#增加將作業時間改成十進位的
worktime10 <-  data_test$work_runtime * 10 / 6
#將進件分鐘改成十進位
gettime10 <- data_test$get_time_minute %>% as.numeric() %>% "*"(10) %>% "/"(6)
#時間加總
get_time10 <-  (gettime10 + worktime10 + (data_test$get_time_hour*100)) %>% round()
#將分鐘還原成60進位
get_time_minute <- get_time10 %>% str_sub(start = -2) %>% as.numeric() %>% "*"(0.6) %>% 
  round() %>% as.character()
get_time_minute <- ifelse(nchar(get_time_minute) == 1,
                          paste0("0",get_time_minute),
                          get_time_minute)
#完成小時
get_time_hour <- ifelse(nchar(get_time10) == 4 ,
                        get_time10 %>% str_sub(1,2),
                        get_time10 %>% str_sub(1,1) %>% paste0("0",.))


#新增完成詳細時間欄位----
data_test$done_time2 <- paste0(get_time_hour,":",get_time_minute)
#新增完成時間
data_test$done_time <- paste0(get_time_hour,":00")
data_test %<>% select(date,type,get_time,get_time2,done_time,done_time2,done,work_runtime)
#新增月份
data_test$get_month <- format(data_test$date,"%m")
#新增年份
data_test$get_year <- format(data_test$date,"%Y")

#增加作業時間的季節性
data_test$work_runtime <- ifelse(data_test$get_month == "01" & data_test$get_year == "2019",
                                 data_test$work_runtime + rnorm(nrow(data_test),6,3),
                                 ifelse(data_test$get_month %in% c("08","09","10","11","12"),
                                        data_test$work_runtime + rnorm(nrow(data_test),4,6),
                                        data_test$work_runtime ))



write.csv(data_test,"data_test.csv",row.names = F,fileEncoding = 'UTF-8')






