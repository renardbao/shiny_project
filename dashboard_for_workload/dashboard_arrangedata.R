#本地端匯入資料
#data_test <- fread("data_test.csv",stringsAsFactors = F,encoding='UTF-8')
#shinyapps.io匯入資料
data_test <- data.table::fread(paste0(getwd(),"/data_test.csv"),
                               header = T,
                               stringsAsFactors = F,
                               encoding='UTF-8')

data_test$date %<>% fast_strptime("%Y-%m-%d") %>% as.Date()

name <- data_test$type %>% unique()
time <- c("08oo","09oo","10oo","11oo","12oo","13oo","14oo","15oo","16oo","17oo") 
#改變時間顯示形式
data_test$get_time %<>% factor(labels = time)
data_test$done_time %<>% factor(labels = c(time,"18oo"))
case_pre <- data.frame(time,case_predict = c(rep(40,5),rep(35,5)))

#專案資料總表-----
data_summary <- data_test %>% 
                  filter(date == "2019-01-21") %>% 
                  group_by(type) %>% summarise("案件量" = n()) %>% 
                  as.data.frame() 
names(data_summary) <- c("專案名稱","進件量")
#案件總量
summary_datatable <- 
  data_summary %>% datatable(filter = 'none',selection = 'none',
                             options = list(searching = FALSE,
                                            paging = FALSE,
                                            info = FALSE,
                                            rowID = FALSE,
                                            ordering = FALSE,
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().tables().body()).css({'font-size': '30px'});",
                                              "$(this.api().tables().body()).css({'color': 'white'});",
                                              "$(this.api().tables().body()).css({'font-family': 'Microsoft JhengHei'});",
                                              "$(this.api().rows().nodes()).css({'background-color': 'black'});", #將底色弄成黑色
                                              "$(this.api().table().header()).css({'font-size': '30px'});",
                                              "$(this.api().table().header()).css({'color': 'white'});",
                                              "$(this.api().table().header()).css({'font-family': 'Microsoft JhengHei'});",
                                              "}")
                             ),
                             rownames = FALSE) %>%
  formatStyle("進件量",target = "cell",
              background = styleColorBar(range(data_summary[,2]),'#4798B3'),
              backgroundSize = '98% 88%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center',
              border = '0px')

#進件/完成件詳細資料----
myplot_data <- list()
for (m in 1:length(name)) {
  myplot_data[[name[m]]] <- 
    merge(data_test %>% 
            filter(date == "2019-01-21",type == name[m]) %>% 
            group_by(get_time) %>%  
            summarise("進件量" = n()) %>% 
            as.data.frame(),
          data_test %>% 
            filter(date == "2019-01-21",type == name[m]) %>% 
            group_by(done_time) %>% 
            summarise("完成量" = n()) %>% 
            as.data.frame() %>% 
            slice(1:10),
          by.x = "get_time",by.y = "done_time") 
  row.names(myplot_data[[name[m]]]) <- myplot_data[[name[m]]]$get_time              
  myplot_data[[name[m]]] %<>% t() %>% as.data.frame() %>% "["(-1,) %>% 
    cbind("案件種類" = c("進件量","完成量"),.)
  myplot_data[[name[m]]][,2:ncol(myplot_data[[name[m]]])] %<>% apply(2,as.character) %>%
    apply(2,as.numeric) %>% as.data.frame()
  myplot_data[[name[m]]][2,2:ncol(myplot_data[[name[m]]])] %<>% "*"(-1)
  #將資料轉成ggplot能讀取
  myplot_data[[name[m]]] %<>% melt(id.vars = "案件種類",
                                   variable.name = "get_time",
                                   value.name = "value") %>% 
    mutate(label = value %>% abs(), #添加顯示label
           position = ifelse(案件種類 == "進件量",
                                 30,-30)) #添加文字位置
  
  #改變level進件量完成量的位置
  myplot_data[[name[m]]]$案件種類 %<>% factor(levels = c("進件量","完成量"))
  #改變get_time的樣式12oo
  myplot_data[[name[m]]]$get_time %<>% factor(labels = time)
}



#平均案件時間----
#過去一年
mean_runtime_df <- list()
for (m in 1:length(name)) {
  mean_runtime_df[[name[m]]] <- 
    data_test %>% filter(type == name[m] & 
                         date >= as.Date("2019-01-21")-365) %>% 
    group_by(date) %>% 
    summarise(mean_wt = mean(work_runtime) %>% 
                        as.vector()) %>% 
    as.data.frame() %>% 
    mutate(mean_zscore = scale(mean_wt) %>% as.vector(),
           mean_fill = ifelse(mean_zscore >= 2,"#ff0000",
                              ifelse(mean_zscore >= 1,"#ff2d2d",
                                     ifelse(mean_zscore >= -1,"#00a600",
                                            "#00bb00"))),
           mean_color = ifelse(mean_zscore >= 1,"#ff0000",
                               "#006600"),
           ymax = 1,
           ymin = 0
    )
  
}

#當日完成率donut plot
data_donut <- list()
for (m in 1:length(name)) {
  data_donut[[name[m]]] <- 
    data_test %>% 
    filter(date == "2019-01-21" & type == name[m]) %>% 
    group_by(done) %>% 
    summarise(sum = n()) %>% 
    arrange(desc(done)) %>%
    mutate(prop = prop.table(sum),
           ymax = cumsum(prop),
           ymin = c(0,head(ymax,n=-1)),
           text = paste0(floor(prop * 100) ,"%")) %>%
    as.data.frame()
}
#今年完成率donut plot2
data_donut2 <- list()
for (m in 1:length(name)) {
  data_donut2[[name[m]]] <- 
    data_test %>% 
    filter(date >= as.Date("2019-01-21") - 365 & type == name[m]) %>% 
    group_by(done) %>% 
    summarise(sum = n()) %>% 
    arrange(desc(done)) %>%
    mutate(prop = prop.table(sum),
           ymax = cumsum(prop),
           ymin = c(0,head(ymax,n=-1)),
           text = paste0(floor(prop * 100),"%")) %>%
    as.data.frame()
}
