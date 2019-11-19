server <- function(input,output,session){
  #sankey
  output$sankey_plot <- renderSankeyNetwork({
    nodepadd_num = 34 #node和node之間的空隙
    font_size = 20 #文字大小
    node_width =  40#node 寬度
    background = "white"
    font_color = "black"
    sn <- sankeyNetwork(Links = sankey_link,
                        Nodes = sankey_node,
                        Source = "source",
                        Target = "target",
                        Value = "value",
                        NodeID = "name",
                        fontSize= font_size,
                        fontFamily = "Microsoft JhengHei",
                        NodeGroup = "group",
                        LinkGroup = "group",
                        colourScale = colorJS,
                        nodeWidth = node_width,
                        sinksRight = F,
                        nodePadding= nodepadd_num
                        )
    #添加哪個node要在左邊
    sn$x$nodes$text_left <- c(rep('l',7),rep('r',6))
    #未掃描件數
    sn$x$nodes$node_value2 <- 
      c(sn$x$links %>% 
          filter(str_detect(group,'未掃')) %>% 
          arrange(source) %>%
          "$"(value) %>% 
          as.character() %>% 
          as.numeric(),
        rep(0,6))
    #總接待數
    sn$x$nodes$node_value_3 <- 
      c(sn$x$links  %>% 
          arrange(source) %>% 
          filter(source <= 6) %>% 
          group_by(source) %>% 
          summarise(value = sum(value)) %>%
          "$"(value),
        rep(0,6)) 
    #未掃描占比
    sn$x$nodes$node_value_4 <- 
      round(sn$x$nodes$node_value2/sn$x$nodes$node_value_3,2)*100
    #node濃度
    sn$x$nodes$opacity <- 
      ifelse(sn$x$nodes$node_value_4 >= 20 ,
             0.9,
             0.3)
    
    sn$x$links$mouseover1 <- sankey_link$mouseover1 
    sn$x$links$mouseover2 <- sankey_link$mouseover2
    sn$x$links %<>% arrange(source,target)
    
    
    onRender(
      sn,
      paste0('
           function(el, x) {
          
           
           
           
           var nodes = d3.selectAll(".node");
           var links = d3.selectAll(".link");
           
           links.style("stroke-opacity", 0.2)
                .on("mouseover", function(d) {
           d3.select(this).style("stroke-opacity", function(d){return 0.7});})
                          .on("mouseout", function(d) {
           d3.select(this).style("stroke-opacity", 0.2);});
           nodes.select("rect").style("cursor", "pointer");
           //拿掉移動
           nodes.on("mousedown.drag", null); // remove the drag because it conflicts
           nodes.on("mouseout", null);
           nodes.on("mouseover", mouseovered)
                .on("mouseout", mouseouted)
                .style("font-weight","bold");
           //background-color
           d3.select("#sankey_plot")
             .select("svg") 
             .style("background-color","',background,'");
           //文字顏色
           d3.selectAll("text").attr("fill", "',font_color,'");
           //調整第一排node 根據占比調整顏色濃度
           d3.selectAll(".node rect")
             .filter(function(d) {return d.text_left == "l";})
             .style("opacity",function(d) { return d.opacity; })
          //add node text(total value)
           nodes.append("text")
                .attr("x",8+',node_width,')
                .attr("text-anchor", "start")
                .attr("y", function(d) { return d.dy / 2; })
                .attr("dy", "1.2em")
                .attr("fill", "',font_color,'")
                .style("font-size",  "20px")
                .text(function(d) { return "( "+d.value + " 件)"; })
                .filter(function(d) { if(d.name == "尚未取件")
                                        return d.name == "尚未取件";
                                     if(d.name == "預估新件")  
                                        return d.name == "預估新件";
                        })
                .text(function(d) { return "( "+ (d.value-1)  + " 件)"; });


           //左邊字調左
           d3.selectAll(".node text")
             .filter(function(d) {return d.text_left == "l";})
             .attr("x", -10)
             .attr("text-anchor", "end");
           //新增尚未掃描的數量
           d3.selectAll(".node")
             .filter(function(d) {return d.text_left == "l";})
             .append("text")
             .attr("x",8+',node_width,')
             .attr("text-anchor", "start")
             .attr("y", function(d) { return d.dy / 2; })
             .attr("dy", "1em")
             .attr("fill", "#DC143C")
             .style("font-size",  "18px")
             .style("font-family","Microsoft JhengHei")
             .text(function(d) { return "( "+d.node_value2 + " 件)"; });
           //新增尚未掃描的百分比
           d3.selectAll(".node")
             .filter(function(d) {return d.text_left == "l";})
             .append("text")
             .attr("x",18+',node_width,')
             .attr("text-anchor", "start")
             .attr("y", function(d) { return d.dy / 2; })
             .attr("dy", "0em")
             .attr("fill", "#DC143C")
             .style("font-size",  "18px")
             .style("font-family","Microsoft JhengHei")
             .text(function(d) { return d.node_value_4 + "%"; });
           
           
           function mouseouted(d, i) {
              links.style("stroke-opacity", 0.2);
           };
           function mouseovered(d, i) {
              links.style("stroke-opacity", function(d1) {
                 if(d1.group == d.name)
                  {
                    return 0.7
                  } else if(d1.mouseover1 == d.name){
                    return 0.7
           
                  } else if(d1.mouseover2 == d.name){
                    return 0.7
           
                  } else
                  {
                    return 0.2
                  };
                     });
             }
           
           
           
           }')
    )
    
  })
  #接待資訊barchar
  output$get_bar <- renderD3({
    r2d3(data = data_to_json(get_bar_data),
         script = "js/bar2.js"
        )
  })
  # #客戶等待表情圖
  # output$text_chart <- renderD3({
  #   r2d3(data = data_to_json(plot_data),
  #        script = "js/text_chart.js"
  #   )
  # })
}
