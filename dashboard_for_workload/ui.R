
ui<- fluidPage(
  tags$head(
    tags$style(
      ".h2,h2 { 
      font-size: 50px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      color: white;
      position: relative;
      top: 15px;};"),
    tags$style(
      "#currentTime{
      color: white;
      font-size: 80px;
      font-weight: bold;
      font-family: Century Gothic;
      position: relative;
      top: 120px;
      };"),
    tags$style(
      "#text1{
      color: white;
      font-size: 22px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      position: relative;
      top: 160px;
      left: 20px;
      };"),
    tags$style(
      "#text2{
      color: white;
      font-size: 22px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      position: relative;
      top: 160px;
      left: 30px;
      };"),
    tags$style(
      "#text3{
      color: white;
      font-size: 22px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      position: relative;
      top: 160px;
      left: 30px;
      };"),
    tags$style(
      "#projecttable{
      position: relative;
      top: 50px;
      border: 0px;};"),
    tags$style(
      "#donut_plot1{
      position: relative;
      top: -150px;
      left: -10px};"),
    tags$style(
      "#donut_plot2{
      position: relative;
      top: -150px;
      left: -10px};"),
    tags$style(
      "#casemeantime_plot{
      position: relative;
      top: -150px;
      left: -10px};"),
    tags$style(
      "#day30bar{
      position: relative;
      top: -280px;
      left: -25px};"),
    tags$style(
      "#myplot_dataplot{
      position: relative;
      top: 75px};"),
    tags$style(
      "#day180_done_area{
      position: relative;
      top: 120px};"),
    tags$style(
      "#projectplot{
      position: relative;
      left: -350px};"),
    tags$style(
      "#case_upper{
      color: white;
      font-size: 15px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      position: relative;
      top: 110px;
      left: -25px
      };"),
    tags$style(
      "#case_lower{
      color: white;
      font-size: 15px;
      font-weight: bold;
      font-family: Microsoft JhengHei;
      position: relative;
      top: 270px;
      left: -25px
      };")
    
    ),
  
  titlePanel("2019-01-21 XX部案件Dashboard")
  ,
  fluidRow(
    column(2,
           DTOutput("projecttable"),
           textOutput("currentTime")
    ),
    
    column(7,
           plotOutput("projectplot",width = "160%" ,height = "700px"),
           plotOutput("myplot_dataplot",width = "100%" ,height = "125px"),
           plotOutput("day180_done_area",width = "102%" ,height = "150px")
    ),
    column(1, textOutput("text1"),plotOutput("casemeantime_plot",width = "125%" ,height = "400px"),
           textOutput("case_upper"),
           textOutput("case_lower")),
    column(1, textOutput("text2"),plotOutput("donut_plot1",width = "125%" ,height = "400px")),
    column(1, textOutput("text3"),plotOutput("donut_plot2",width = "125%" ,height = "400px")),
    fluidRow(
      fixedRow(column(3,plotOutput("day30bar",width = "100%" ,height = "900px")))
      
    )
  ),
  
  
  setBackgroundColor(color = "black")
    )