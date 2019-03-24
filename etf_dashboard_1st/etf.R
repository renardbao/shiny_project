library(data.table)
library(magrittr)
library(reshape2)
library(stringr)
library(dplyr)
library(urca)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
options(scipen=999)
etf <- fread("tetfp.csv",sep = ",",stringsAsFactors = F)
names(etf) <- c("code","date","name","open","high","low","close","volume")
etf$code %<>%  str_replace_all(" ","")
#for data list query
etf.code <- unique(etf$code) 
#for ui choice list
etf.code1 <- paste0("etf",etf.code)
#台股指數
twi <- fread("twii.csv",sep = ",",header = T,stringsAsFactors = F)
names(twi) <- c("date" , "twi")
twi$date %<>% as.Date(format="%Y-%m-%d")
#美股指數
sp500 <-  fread("sp500.csv",sep = ",",header = T,stringsAsFactors = F)
names(sp500) <- c("date" , "sp500")
sp500$date %<>% as.Date(format="%Y-%m-%d")
#三種石油以及匯率
oil <-  fread("petrol.csv",sep = ",",header = T,stringsAsFactors = F)
oil$date %<>% as.Date(format="%Y-%m-%d") 
names(oil)[5] <- "change.rate"
oil <- arrange(oil,date)
#多創一個變數只包含月份日期
#用fast_strptime比較快
etf$date %<>% as.character()%>% fast_strptime("%Y%m%d") %>% as.Date()
etf$open %<>% as.numeric(2)
etf$high %<>% as.numeric(2)
etf$low %<>% as.numeric(2)
etf$close %<>%  as.numeric(2)
etf$volume %<>%  str_replace_all(",","") %>% as.numeric(2)


