source("etf.R")

#transform data for ggplot2
etf.data.melt <- list()
for (i in 1:length(etf.code)){
  etf.data.melt[[paste0("etf",etf.code[i])]] <- 
    data.frame(
      date = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% "["(,"date") ,
      open = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"open"),
      close = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"close") ,
      high = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"high") ,
      low =  merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>%
        "["(,"low") ,
      twi = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"twi") ,
      sp500 = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"sp500"),
      brent = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"brent") ,
      change.rate = merge(etf %>% filter(code == etf.code[i]),sp500,by = "date") %>% 
        merge(twi,by = "date") %>% merge(oil,by = "date") %>% 
        "["(,"change.rate")
      
    )  %>% melt(id.vars = "date") %>% group_by(variable)%>% mutate(scale_value = scale(value)) %>% 
    as.data.frame()
  
  
}



ui <- 
  fluidPage(
    tags$head(
      #不顯示error
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$style(
        "label{
        color: white;
        font-size: 30px;
        font-weight: bold;
        font-family: Microsoft JhengHei;
        }"
      ),
      tags$style(
        ".irs-from, .irs-to, .irs-single{
               font-size: 20px;
               font-weight: bold;
               font-family: Microsoft JhengHei;
        }"
      ),
      tags$style(
        ".help-block{
               font-size: 14px;
               font-weight: bold;
               font-family: Microsoft JhengHei;
               color:white;
        }"
      ),
      tags$style(
        ".selectize-control{
               font-size: 20px;
               font-weight: bold;
               font-family: Microsoft JhengHei;
               
        }"
      ),
      tags$style(
        ".checkbox label{
               font-size: 20px;
               font-weight: bold;
               font-family: Microsoft JhengHei;
               
        }"
      ),
      tags$style(
        ".h2,h2{
               font-size: 40px;
               font-weight: bold;
               font-family: Microsoft JhengHei;
               color:white;
               
        }"
      )
      
    ),
    setBackgroundColor(color = "black"),
    titlePanel("ETF走勢圖"),
    fluidRow(
      plotlyOutput("etfplot", "100%", "600px") %>% withSpinner(color = "white",type = 5)
    ),
    fluidRow(
      column(3,
             selectInput("etf.code","ETF代碼",choices = etf.code1),
             hr(),
             helpText("Made by renardbao"),
             br()
      ),
      column(3,
             checkboxGroupInput("select",
                                label = "顯示",
                                choices = c(1)
             )
             
      ),
      column(4,
             sliderInput("date_range","時間範圍",
                         min = Sys.Date(),max = Sys.Date(),
                         value = c(Sys.Date(),Sys.Date())
             )
             
      )
    )
  )
  



server <- function(input,output,session){
  
  
  
  observeEvent(input$etf.code,{         
    updateCheckboxGroupInput(session,
                             inputId  = "select",
                             choices  = etf.data.melt[[input$etf.code]][,"variable"] %>% unique(),
                             selected = etf.data.melt[[input$etf.code]][,"variable"] %>% unique()
    )
  })
  
  observeEvent(input$etf.code,{         
    updateSliderInput(session,
                      inputId  = "date_range",
                      min = etf.data.melt[[input$etf.code]][,"date"] %>% min,
                      max = etf.data.melt[[input$etf.code]][,"date"] %>% max,
                      value = c( etf.data.melt[[input$etf.code]][,"date"] %>% min,
                                 etf.data.melt[[input$etf.code]][,"date"] %>% max)
    )
  })
  
  
  
  
  output$etfplot <- renderPlotly({
    alpha.var <- ((etf.data.melt[[input$etf.code]][,"variable"] %>% unique() %in% input$select)*9+1)/10
    
    p<- 
      ggplot(data = etf.data.melt[[input$etf.code]] %>% 
               filter(date >= input$date_range[1]& date <= input$date_range[2]),
             aes(label = value)) + #aes for plotly show value
        geom_line(aes(x = date , y = scale_value ,color = variable,alpha = variable),
                  size = 1.3) +
        scale_alpha_manual(values = alpha.var) +
        labs(title = input$etf.code %>% str_sub(4,nchar(input$etf.code)))+
        theme(legend.key.width = unit(2,"cm"),
              legend.text = element_text(size = 14,colour = "white"),
              legend.key = element_rect(fill = 'transparent',color = "transparent"),
              legend.title = element_text(size = 18,colou = "white"),
              legend.background = element_rect(fill = "transparent" ),
              axis.text.x = element_text(size = 24,face = "bold",color = "white"),
              axis.title.x = element_text(size = 40,face = "bold",color = "white"),
              axis.text.y = element_text(size = 24,face = "bold",color = "white"),
              axis.title.y = element_text(size = 40,face = "bold",color = "white"),
              panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black",color = "black"),
              plot.title = element_text(hjust = 0.5,size = 40, face = "bold",color = "white"))
    
    ggplotly(p)
  })
  
}





shinyApp(ui = ui, server = server)
