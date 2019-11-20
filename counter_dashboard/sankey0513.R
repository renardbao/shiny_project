library(networkD3)
library(dplyr)
library(magrittr)
library(stringr)
source("counter_data_sankey.R",encoding = 'UTF-8')

#sankey data prepare----
#第一層 櫃台
counter_sankeydata1 <- counter_data_sankey %>% 
  group_by(counter_location,scan) %>% 
  summarise(value = n()) %>% 
  as.data.frame()
colnames(counter_sankeydata1) <- c('source',"target",'value')
counter_sankeydata1 %<>% arrange(source) 
#第二層 掃描
counter_sankeydata2 <- counter_data_sankey %>%
  group_by(scan,accept) %>% 
  summarise(value = n()) %>%
  as.data.frame()
colnames(counter_sankeydata2) <- c('source',"target",'value')
##去掉na
counter_sankeydata2 <- counter_sankeydata2[-which(counter_sankeydata2$target %>% is.na()),]

#node 
counter_node <- data.frame(name = c(counter_sankeydata1$source,
                                    counter_sankeydata1$target,
                                    counter_sankeydata2$target) %>% unique() ,
                           stringsAsFactors = F
)

#sankey_index
counter_index <- counter_node %>% mutate(index= 0:(nrow(counter_node)-1))

#group 
counter_node$group <- counter_node$name

#node color
counter_node$color <- c(rep('#002FA7',7), #六個地區&客業櫃
                        "#FF4500",#作整未掃
                        "#FF4500",#理賠未掃
                        "#FF4500",#行助未掃
                        "#C0C0C0",#已掃描
                        "#FF7300",#尚未受理
                        "#A9A9A9"#已受理
)



#group色表
counter_color <- counter_node[,c("group","color")]

#links 
counter_link <- base::rbind(counter_sankeydata1,
                            counter_sankeydata2)
#後面控制滑鼠移過去的欄位
counter_link$a <- counter_link$source
counter_link$b <- counter_link$target

counter_link$group <- counter_link$target
counter_link$color <- counter_color$color[match(counter_link$group, counter_color$group)]
#將source和target依照index代換編碼
counter_link %<>% mutate_at(.vars = vars(source,target),
                            .funs = funs(counter_index$index[match(., counter_index$name)]))


colors <- paste(sapply(counter_node$color, function(x) { paste0("d3.rgb(", paste(c(col2rgb(x),0.5), collapse = "," ), ")") }), collapse = ", ")
colorJS <- paste0('d3.scaleOrdinal([', colors, '])')
colorJS <- sprintf('d3.scaleOrdinal().domain(%s).range(%s)',
                   jsonlite::toJSON(counter_color$group),
                   jsonlite::toJSON(counter_color$color)
) %>% networkD3::JS()


mysankey <- function(counter_link,counter_node,colorJS,
                     nodepadd_num,font_size = 40,node_width = 60,
                     background = 'white',font_color = 'black'){
  sn <- sankeyNetwork(Links = sankey_link, Nodes = sankey_node,
                      Source = "source", Target = "target",
                      Value = "value", NodeID = "name",
                      fontSize= font_size,fontFamily = "Microsoft JhengHei",
                      NodeGroup = "group", LinkGroup = "group",
                      colourScale = colorJS,nodeWidth = node_width,sinksRight = F,nodePadding=nodepadd_num)
  #添加哪個node要在左邊
  
  sn$x$nodes$text_left <- c(rep('l',7),rep('r',6))
  #未掃描件數
  sn$x$nodes$node_value2 <- c(sn$x$links %>% filter(str_detect(group,'未掃')) %>% arrange(source) %>% 
                                "$"(value) %>% as.character() %>% as.numeric(),
                              rep(0,6))
  #總接待數
  sn$x$nodes$node_value_3 <- c(sn$x$links  %>% arrange(source) %>% filter(source <= 6) %>% 
                                 group_by(source) %>% summarise(value = sum(value)) %>% "$"(value),
                               rep(0,6)) 
  #未掃描占比
  sn$x$nodes$node_value_4 <- round(sn$x$nodes$node_value2 / sn$x$nodes$node_value_3,2) *100
  #node濃度
  sn$x$nodes$opacity <- ifelse(sn$x$nodes$node_value2 >= 5 ,0.9,0.3)
 
  sn$x$links$a <- counter_link$a
  sn$x$links$b <- counter_link$b
  sn$x$links %<>% arrange(source,target)


  htmlwidgets::onRender(
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
           //nodes.on("mousedown.drag", null); // remove the drag because it conflicts
           //nodes.on("mouseout", null);
           nodes.on("mouseover", mouseovered)
                .on("mouseout", mouseouted)
                .style("font-weight","bold");
           //background-color
           d3.select("body").style("background-color", "',background,'");
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
                  } else if(d1.a == d.name){
                    return 0.7
           
                  } else if(d1.b == d.name){
                    return 0.7
           
                  } else
                  {
                    return 0.2
                  };
                     });
             }
           
           
           
           }')
    )
  
  }
#mysankey(counter_link,counter_node,colorJS,34,20,40,"rgb(7, 1, 25)","white")
mysankey(counter_link,counter_node,colorJS,
         nodepadd_num = 34, #node和node之間的空隙
         font_size = 20, #文字大小
         node_width =  40, #node 寬度
         background = "transparent",
         font_color = "black")


sn <- sankeyNetwork(Links = counter_link, Nodes = counter_node,
                    Source = "source", Target = "target",
                    Value = "value", NodeID = "name",fontFamily = "Microsoft JhengHei",
                    NodeGroup = "group", LinkGroup = "group",
                    colourScale = colorJS)

sn$x$links$a <- counter_link$a
sn$x$links$b <- counter_link$b

sn$x$links
