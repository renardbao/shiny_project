showtext_auto() #讓ggplot2顯示正確的中文
server <- function(input,output,session){
  i = 0
  #source("dashboard_arrangedata.R",encoding = "UTF-8")
  source(paste0(getwd(),"/dashboard_arrangedata.R"),encoding = "UTF-8")
  
  autoInvalidate <- reactiveTimer(60000)#一分鐘更新一次
  
  if(i == length(name))i=1
  observe({
    
    autoInvalidate()
    i <<- i + 1
    if(i == length(name)) i<<-1
    data_test_filter <- data_test %>% filter(date == "2019-01-21",type == name[i])
    output$projectplot <- renderPlot({
      #主圖----
      my_plot <- data_test_filter %>% 
        ggplot() + 
        geom_bar(aes(x = get_time,fill = done),
                 position = "stack", 
                 width=0.87,
                 show.legend = T) +
        geom_bar(data = case_pre ,     #預估完成量
                 aes(x = time,y = case_predict,fill = "case_predict"),
                 stat = 'identity',
                 width = 0.55,
                 show.legend = T) +
        scale_fill_manual(name = "",
                          values = c(  "white","#FF2E63", "#08D9D6"),
                          labels = c("預估完成量","未完成","已完成"))+
        coord_polar("x") +
        labs(x = name[i],y = NULL) +
        scale_y_continuous(labels = NULL,
                           breaks = seq(1,max(data_summary$進件量),by=10)) +
        theme(legend.text = element_text(size = 20,color = "white"),
              legend.title = element_text(size = 20),
              legend.key.size = unit(3,"line"),
              legend.key = element_rect(color = 'black'),
              legend.background = element_rect(fill = "transparent" ),
              legend.position = c(0.99, 0.02),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 24,face = "bold",color = "white"),
              axis.title.x = element_text(size = 55,face = "bold",color = "white"),
              panel.grid.major.y =element_blank(), #去掉網線
              panel.grid.major.x = element_line(color = "black",linetype = "dashed",size = 1),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",color = "transparent"),
              plot.margin = margin(0.1,0.01,0.1,0.1, "cm")
        ) 
      
      my_plot + geom_hline(yintercept = seq(1,max(ggplot_build(my_plot)$data[[1]]$count),10),
                           color = 'white', linetype="dashed") 
    }, bg="transparent")#將背景透明
    #左邊各專案案件總量
    output$projecttable <- renderDT({
      summary_datatable %>%
        formatStyle("專案名稱",target = "cell",
                    background = styleEqual(name[i],'#c48888'),
                    border = '0px'
        )
      
    })
    
    #進件/完成件詳細資料----
    output$myplot_dataplot <- renderPlot({
      
      myplot_data[[name[i]]] %>% 
        ggplot(aes(x = get_time,y = value,fill = 案件種類,group = 案件種類)) + 
        geom_bar(aes(alpha = 案件種類,color = 案件種類),
                 stat = "identity",
                 width = 0.75) + 
        geom_text(aes(y = position,label = label),color = "white",size=10) + 
        labs(x = NULL,y = NULL) +
        scale_alpha_manual(values = c(1,0.7))+
        scale_fill_manual(values = c("black","#08D9D6"))+
        scale_color_manual(values = c("steelblue","#08D9D6")) + 
        theme(legend.text = element_text(size = 24,color = "white"),
              legend.key.size = unit(2,"line"),
              legend.key = element_rect(color = 'black'),
              legend.background = element_rect(fill = "transparent" ),
              legend.title = element_blank(),
              legend.position = "left",
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size=24,color = "white"),
              axis.ticks=element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",
                                             color = "transparent"))
      
    }, bg="transparent")
    #半年完成率
    output$day180_done_area <- renderPlot({
      
      
      data_test %>%
        filter(type == name[i] & date >= as.Date("2019-01-21")-180) %>%
        ggplot() +
        geom_area(aes(x = date,fill = done,alpha = done,color = done),
                  stat = "count",
                  show.legend = F)+
        scale_x_date(date_breaks = "1 month",date_labels = "%Y/%m") +
        scale_alpha_manual(values = c(1,0))+
        scale_color_manual(values = c("transparent","steelblue"))+
        scale_fill_manual(values = c("red","transparent"))+
        labs(x = NULL,y = NULL) +
        theme(panel.grid=element_blank(),
              axis.text.y=element_text(size = 16,color = "white"),
              axis.text.x = element_text(size=24,color = "white"),
              axis.ticks=element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",
                                             color = "transparent")
              )
      
    }, bg="transparent")
    #donutplot 當日完成率----
    output$donut_plot1 <- renderPlot({
      
      data_donut[[name[i]]] %>% 
        ggplot(aes(fill=done, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        scale_fill_manual(values = c("#FF2E63", "#08D9D6"))+
        geom_rect(show.legend = F) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        labs(x = NULL,y = NULL) +
        theme(panel.grid=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",color = "transparent")) +
        annotate("text", x = 2, y = 0, 
                 label =  data_donut[[name[i]]][1,"text"],
                 size = 11,
                 color = "white")
    }, bg="transparent")
    #donutplot 當年完成率----
    output$donut_plot2 <- renderPlot({
      
      data_donut2[[name[i]]] %>% 
        ggplot(aes(fill=done, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
        geom_rect(show.legend = F) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        labs(x = NULL,y = NULL) +
        theme(panel.grid=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",color = "transparent")) +
        annotate("text", x = 2, y = 0, label = data_donut2[[name[i]]][1,"text"],
                 size = 11,
                 color = "white")
    }, bg="transparent")
    #案件平均完成時間----
    output$casemeantime_plot <- renderPlot({
      #平均案件時間
      
      mean_runtime_df[[name[i]]] %>% 
        ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3.2)) +
        geom_rect(show.legend = F,
                  fill =  mean_runtime_df[[name[i]]] %>% 
                    filter(date == "2019-01-21") %>% 
                    "["(,"mean_fill")) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        annotate("text", x = 2, y = 0, size = 13,
                 label =  mean_runtime_df[[name[i]]] %>% 
                   filter(date == "2019-01-21") %>% 
                   "["(,"mean_wt") %>% round() %>% 
                   paste0('分'),
                 color = "white") +  
        labs(x = NULL,y = NULL) +
        theme(panel.grid=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent",color = "transparent"))
      
    }, bg="transparent")
    
    #30day barchart-----
    #時間固定在2019-01-21
    output$day30bar <- renderPlot({
      data_day30 <- data_test %>% filter(type == name[i] & date >= as.Date("2019-01-21")-30) 
      
      grid.arrange(
        mean_runtime_df[[name[i]]] %>% 
          filter(date >= as.Date("2019-01-21")-30) %>% 
          ggplot() + 
          geom_bar(aes(x = date,y = mean_wt,fill = mean_zscore),
                   width = 0.8,stat = "identity")+
          scale_x_date(breaks = seq(max(data_day30$date),min(data_day30$date),by = -1),
                       date_labels = "%m/%d") + 
          labs(x = NULL,y = NULL)+
          scale_fill_gradient2(name = "",labels = NULL,
                               midpoint=1,mid= "#79ff79",low = "green",high = "red")+
          theme(panel.grid=element_blank(),
                legend.key = element_rect(color = 'black'),
                legend.background = element_rect(fill = "transparent" ),
                legend.position = "left",
                legend.key.size = unit(2,"line"),
                axis.text.y=element_blank(),
                axis.text.x=element_text(size = 15,color = "white"),
                axis.ticks=element_blank(),
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent",color = "transparent"),
                plot.margin = margin(0.1,0.2,0.1,0.1, "cm"))+#調整繪圖區大小
          coord_flip(),
        #30barchart
        data_day30 %>% 
          ggplot() + 
          geom_bar(aes(x = date,fill = done),stat = "count",width = 0.8,show.legend = F)+
          scale_x_date(breaks = seq(max(data_day30$date),min(data_day30$date),by = -1),
                       date_labels = "%m/%d") + 
          labs(x = NULL,y = NULL) +
          theme(panel.grid=element_blank(),
                axis.text.y=element_text(size = 20,color = "white"),
                axis.text.x=element_text(size = 15,color = "white"),
                axis.ticks=element_blank(),
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent",color = "transparent"))+
          scale_y_reverse() + coord_flip(),
        layout_matrix = matrix(c(1,2,2),nrow = 1,ncol = 3)
      )
      
    }, bg="transparent")
    #30daybar 圖例
    
    output$case_upper <- renderText({
      paste0(max(mean_runtime_df[[name[i]]]$mean_wt) %>% round,"分鐘")
    })
    output$case_lower <- renderText({
      paste0(min(mean_runtime_df[[name[i]]]$mean_wt) %>% round,"分鐘")
    })
  })
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste(Sys.time() %>% format("%H:%M:%S"))
  })
  output$text1 <- renderText({
    "今日案件耗時"
  })
  output$text2 <- renderText({
    "今日完成率"
  })
  output$text3 <- renderText({
    "今年完成率"
  })
  
}
