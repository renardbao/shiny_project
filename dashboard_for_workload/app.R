library(data.table)#fread()快速載入資料
library(shiny)
library(dplyr)
library(magrittr)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(gridExtra)
library(reshape2)
library(stringr)
library(htmlwidgets)
library(htmltools)
library(devtools)
library(grDevices)
library(lubridate)#fast_strptime() 快速轉換時間格式
library(showtext)#讓ggplot2正確顯示中文
#shinyapps.io
source_url("https://raw.githubusercontent.com/renardbao/shiny_project/master/dashboard1/ui.R", encoding = "UTF-8")
source_url("https://raw.githubusercontent.com/renardbao/shiny_project/master/dashboard1/server.R",encoding = "UTF-8")

#本地端測試
#source("ui.R",encoding = "UTF-8")
#source("server.R",encoding = "UTF-8")


shinyApp(ui = ui, server = server)
