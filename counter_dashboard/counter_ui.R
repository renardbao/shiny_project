ui<- fluidPage(
  
  #setBackgroundColor(color = "grey"),
  tags$head(
    tags$style(
      ".row{
        background-color: grey;
      };
      "
    ),
    tags$style(
      ".h2,h2 { 
        font-size: 50px;
        font-weight: bold;
        font-family: Microsoft JhengHei;};
      "
    ),
    tags$style(
      "[class^='col'] { 
        padding-top: 15px;
        padding-bottom: 15px;};
      "
    )
  ),
  
  titlePanel("理賠場控系統")
  ,
  fluidRow(
    column(8,sankeyNetworkOutput('sankey_plot')),
    column(4,d3Output("get_bar",height = '500px'))
  )#,
  # fluidRow(
  #   column(6,d3Output("text_char",height = '500px')),
  #   
  # )
  
)

