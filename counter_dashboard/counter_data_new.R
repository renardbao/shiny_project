library(dplyr)
library(magrittr)
library(reshape2)
library(stringr)

source('function.R')
#rawdata----
#前線人員
#B櫃台人員 C客/業櫃人員
man_id <- c(paste0("B",seq(1001,1017)),paste0("C",seq(1001,1004)))
#二線人員
man_id2 <- c(paste0("A",seq(1001,1005)))
#一天案件量
n_case <- 150
counter_name <- c('信義','建北','南京','文心',
                  '台南','新興','客/業櫃')

set.seed(1234)
counter_data <- 
  data.frame(stringsAsFactors = F,
             man_id = sample(man_id,n_case,replace = T),
             #掃描
             scan = sample(c(T,F),n_case,replace = T,
                           prob = c(0.8,0.2)),
             #受理
             accept = sample(c(T,F),n_case,replace = T,
                             prob = c(0.8,0.2)),
             #案件類型
             type = sample(c('m','d'),n_case,replace = T,
                           prob = c(0.8,0.2))
             )
                              




#根據人員分櫃台
counter_data$location <- NA

counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1001,1004))),
  'location'] <- '信義'
counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1005,1006))),
  'location'] <- '建北'

counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1007,1008))),
  'location'] <- '南京'

counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1009,1012))),
  'location'] <- '文心'
counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1013,1014))),
  'location'] <- '台南'
counter_data[
  which(
    counter_data$man_id %in% 
      paste0('B',seq(1015,1017))),
  'location'] <- '新興'
                                     
counter_data[
  which(
    counter_data$man_id %>%
      substr(1,1) == 'C'),
  'location'] <- '客/業櫃'
#location revalue to english
location_w <- paste0("a",1:length(counter_name))
names(location_w) <- counter_name
counter_data$location_w <- plyr::revalue(counter_data$location,
                                        location_w)

#scan_wording
counter_data$scan_w <- NA
counter_data[
  which(counter_data$scan == T),
  'scan_w'] <- '已掃描'

counter_data[
  which(counter_data$scan == F),
  'scan_w'] <- '行助未掃'


counter_data[
  which(counter_data$scan == F & 
        counter_data$location %in% c('信義','建北','文心','新興')),
  'scan_w'] <- '作整未掃'

counter_data[
  which(counter_data$scan == F & 
          counter_data$location %in% c('南京','台南')),
  'scan_w'] <- '理賠未掃'

#修正scan和accept階層關係
counter_data$accept <- ifelse(counter_data$scan == F,
                              F,counter_data$accept)
counter_data$accept_w <- 
  ifelse(counter_data$accept == F,'未受理','已受理')

#假定時間
now_time <- as.POSIXct("16:00:00",format = '%H:%M:%S')

#抽號碼牌時間
counter_data$enter_t <- time_random(rep('08:30:00',n_case),
                                seed = 1111)
#等待時間
counter_data$wait_time <- runif(n_case,1,18) %>% round

#服務起始時間
counter_data$start_t <- time_sum(counter_data$enter_t,
                                 sum_value = counter_data$wait_time)
#服務時間
set.seed(1234)
counter_data$serve_time <- runif(n_case,10,45) %>% round

#服務結束時間  
counter_data$end_t <- time_sum(counter_data$start_t,
                               sum_value = counter_data$serve_time)


#客戶服務進行中的階段
counter_data$serve_phase <- ifelse(now_time > as.POSIXct(counter_data$end_t,format = '%H:%M:%S'),
                                  'finish',
                                  ifelse(now_time > as.POSIXct(counter_data$start_t,format = '%H:%M:%S'),
                                         'serve',
                                         ifelse(now_time > as.POSIXct(counter_data$enter_t,format = '%H:%M:%S'),
                                                'wait',NA)))

#導入指定時間後
#等待時間
counter_data$wait_time_new <- as.numeric(difftime(now_time,as.POSIXct(counter_data$enter_t,format = '%H:%M:%S'),units = 'mins')) %>% 
  round
counter_data$wait_time_new <- ifelse(counter_data$serve_phase %in% c('finish','serve'),
                                     counter_data$wait_time,
                                     ifelse(is.na(counter_data$serve_phase),NA,
                                            counter_data$wait_time_new))
                                    

#服務時間
counter_data$serve_time_new <- as.numeric(difftime(now_time,as.POSIXct(counter_data$start_t,format = '%H:%M:%S'),units = 'mins')) %>% 
  round()
counter_data$serve_time_new <- ifelse(counter_data$serve_phase == 'finish',
                                      counter_data$serve_time ,
                                      ifelse(counter_data$serve_phase %in% c('wait',NA),
                                             NA,counter_data$serve_time_new))
counter_data$serve_phase <- ifelse(counter_data$serve_phase == 'wait' & 
                                     counter_data$wait_time_new >= 10 ,
                                   'wait_long',counter_data$serve_phase)



#sankey data-----
#第一層 櫃台
sankeydata1 <- counter_data %>% 
  group_by(location,scan_w) %>% 
  summarise(value = n()) %>% 
  as.data.frame()
colnames(sankeydata1) <- c('source',"target",'value')
#第二層 掃描
sankeydata2 <- counter_data %>%
  group_by(scan_w,accept_w) %>% 
  summarise(value = n()) %>%
  as.data.frame()
colnames(sankeydata2) <- c('source',"target",'value')
#node 
sankey_node <- data.frame(name = c(sankeydata1$source,
                                   sankeydata1$target,
                                   sankeydata2$target) %>% 
                            unique() ,
                           stringsAsFactors = F
)

#node group 
sankey_node$group <- sankey_node$name
#node color
sankey_node$color <- c(rep('#002FA7',7), #六個地區&客業櫃
                       "#FF4500",#作整未掃
                       "#FF4500",#理賠未掃
                       "#FF4500",#行助未掃
                       "#C0C0C0",#已掃描
                       "#FF7300",#尚未受理
                       "#A9A9A9"#已受理
)


#sankey_index
sankey_index <- sankey_node %>% 
  mutate(index= 0:(nrow(sankey_node)-1))
#group色表
sankey_color <- sankey_node[,c("group","color")]

#links 
sankey_link <- base::rbind(sankeydata1,
                           sankeydata2)
#後面控制滑鼠移過去顯示的欄位
sankey_link$mouseover1 <- sankey_link$source
sankey_link$mouseover2 <- sankey_link$target

#links顏色
sankey_link$group <- sankey_link$target
sankey_link$color <- sankey_color$color[
  match(sankey_link$group,
        sankey_color$group)]
#將source和target依照index代換編碼
sankey_link %<>% mutate_at(.vars = vars(source,target),
                           .funs = funs(sankey_index$index[
                             match(.,sankey_index$name)
                             ])
                           )

colors <- paste(sapply(sankey_node$color,
                       function(x) { 
                         paste0("d3.rgb(",
                                paste(c(col2rgb(x),0.5), 
                                      collapse = "," ),
                                ")") }), 
                collapse = ", ")
colorJS <- paste0('d3.scaleOrdinal([', colors, '])')
colorJS <- sprintf('d3.scaleOrdinal().domain(%s).range(%s)',
                   jsonlite::toJSON(sankey_color$group),
                   jsonlite::toJSON(sankey_color$color)
) %>% networkD3::JS()




#先整理其他圖表要用到的TABLE資料----
get_bar_data <- counter_data %>% 
  filter(location !='客/業櫃' ) %>%
  group_by(location,type) %>%
  summarise(value = n()) %>% 
  arrange(type) %>% 
  rename(variable = type) %>% as.data.frame()

get_bar_data %<>% rbind(
  counter_data %>% 
    filter(location !='客/業櫃' ) %>%
    group_by(location) %>%
    summarise(value = n()) %>% 
    mutate(variable = 'get_num') %>% 
    select(location,variable,value) %>% as.data.frame()
)
get_bar_data$variable <- plyr::revalue(get_bar_data$variable,
                                       c('d' ='get_num_d','m' = 'get_num_m'))

get_bar_data$location_w <- plyr::revalue(get_bar_data$location,
                                         location_w)
get_bar_data %<>% arrange(match(variable,
                                c('get_num','get_num_m','get_num_d'))
)

?arrange

