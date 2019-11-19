//encoding = big5
/* location_w table
信義    建北    南京    文心    台南    新興 客/業櫃 
   "a1"    "a2"    "a3"    "a4"    "a5"    "a6"    "a7" 
*/


var padding = {
  top : 20,
  right : 20,
  bottom : 20,
  left : 70
};


//x軸 axis高度
var x_height = 40;
//一個bar高度
//包含padding
var bar_height = (height - x_height)/2;
//比例尺
var bar_scale = d3.scaleLinear()
                  .domain([0,d3.max(data,function(d){
                     return d.value})])
                  .range([0,bar_height - padding.top]);



var data_unique =d3.map(data,function(d){
                     return d.location})
                     .keys();
var data_unique2 = d3.map(data,function(d){
                     return d.location_w})
                     .keys();
                     
var data_location_table = new Array();
                     
var data_length = d3.map(data,function(d){
                     return d.location})
                     .keys().length;



for(var i=0;i < data_length;i++){
   data_location_table.push(
      {location:data_unique[i],
       location_w:data_unique2[i]
      });

}



var barwidth = Math.ceil((width-padding.right - padding.left)/data_length);


//bar條間的空隙
var space = 15;


//define bar
var bar = svg.selectAll('svg')
             .data(data)
             .enter()
             .filter(function(d){
                 if(d.variable.match('get_num'))
                     return true;
                 else
                     return false;
                       });
                       
                       
//define color
var color = [
   {
      type:"已接待",
      color:"steelblue"
   },
   {
      type:"醫療件",
      color:"yellow"
   },
   {
      type:"死殘件",
      color:"orange"
   }
] ;  

       



//bar
bar.append('rect')
   .attr('id',function(d){
      return d.location_w;
   })
   .attr('class',function(d){
      if(d.variable == 'get_num'){
         return  'get_bar b1';
      } else {
         return  'get_bar b2';
      }
   }
  )
   .style('fill',function(d){
       if(d.variable == 'get_num'){
          return color[0].color;
       } else if(d.variable == 'get_num_m'){
          return color[1].color;
       } else{
          return color[2].color;
       }})
   .style('fill-opacity',0.7)
   .attr('height', function(d){return bar_scale(d.value);})
   .attr('width', function(d) { 
      if(d.variable == 'get_num'){
          return barwidth - space;
      } else{
          return (barwidth - space)/2;
      }})
   .attr('x', function(d,i){
      if(d.variable == 'get_num'){
         return (padding.left + i * barwidth);
      } else if(d.variable == 'get_num_m') {
         return padding.left + (i- data_length) * barwidth ;
      } else{
        return padding.left + (i- (2*data_length)) * barwidth + (barwidth - space)/2 ;
      }})
   .attr('y', function(d) { 
       if(d.variable == 'get_num'){
          return bar_height - bar_scale(d.value) ;
       }else {
          return bar_height + x_height;
       }
          });

//bar text
bar.append('text')
   .attr('id',function(d){
        return d.location_w;})
   .attr('class',function(d){
      if(d.variable == 'get_num'){
         return  'get_bar_t b1';
      } else {
         return  'get_bar_t b2';
      }
   }
  )
   .style('fill','black')
   .style('font-family','Microsoft JhengHei')
   .style('font-weight','bold')
   .attr('text-anchor','middle')    
   .style('font-size',function(d){
      if(d.variable == 'get_num'){
         return '18px';
      } else {
         return '14px';
      }
   })
   .attr('x', function(d,i){
      if(d.variable == 'get_num'){
         return padding.left + i * barwidth ;
      } else if(d.variable == 'get_num_m'){
         return padding.left + (i- data_length) * barwidth;
      } else{
         return padding.left + (i- 2*data_length) * barwidth;
      }
          
      })
   .attr('y', function(d) {
      if(d.variable == 'get_num'){
         return bar_height - bar_scale(d.value) ;
      } else {
         return bar_height + x_height + bar_scale(d.value); 
      }})
   .attr('dx',function(d){
      if(d.variable == 'get_num'){
         return (barwidth - space)/2;
      } else if(d.variable == 'get_num_m') {
         return (barwidth - space)/2 - barwidth/4 ;
      } else{
          return (barwidth - space)/2 + barwidth/4;
      }
   })
   .attr('dy',function(d){
      if(d.variable =='get_num'){
         return '-0.5em';
      } else {
         return '1em';
      }})
   .text(function(d){
      return d.value;
   });


// bar yaxis text
//已接待
svg.append('text')
     .style('fill','black')
     .style('font-family','Microsoft JhengHei')
     .style('font-weight','bold')
     .style('writing-mode','tb')
     .style('font-size','24px')
     .attr('x',padding.left/2)
     .attr('y',(bar_height - padding.top)/3+padding.top)
     .text('已接待');
//進件類型
svg.append('text')
     .style('fill','black')
     .style('font-family','Microsoft JhengHei')
     .style('font-weight','bold')
     .style('writing-mode','tb')
     .style('font-size','24px')
     .attr('x',padding.left/2)
     .attr('y',bar_height + x_height +(bar_height - padding.top)/4 )
     .text('進件類型');
     
  
var markstep = 80;     
var mark = svg.selectAll(".gmark")
              .data(color)
              .enter()
              .append("g")
              .filter(function(d){
                 if(d.type == '已接待')
                   return false;
                 else
                   return true;
              })
              .attr("transform",function(d,i){
                return "translate(" + (padding.left + i * markstep ) + "," + 
                       (height - padding.bottom -5 ) + ")";
              });


//mark rect              
mark.append("rect")
    .attr("width","20")
    .attr("height","20")
    .attr("fill",function(d){
       return d.color;
    });
//mark text

mark.append("text")
    .attr("dx","25")
    .attr("dy","1em")
    .attr("fill","black")
    .style('font-family','Microsoft JhengHei')
    .style('font-weight','bold')
    .text(function(d){
       return d.type;
    });

//define xaxis mouse over       
var xaxis_mover = function(d,i){
   var id = d3.select(this)
              .selectAll('text')
              .attr('id');
        d3.select(this)
          .selectAll("rect")
          .style("fill","black")
          .style("fill-opacity",0.7);
        d3.select(this)
          .selectAll("text")
          .style("fill","white");
        d3.select(this)
          .style("cursor", "pointer");
      //調大bar的文字    
        svg.selectAll('#'+id)
           .filter('.get_bar_t')
          .style('font-size',function(d){
          if(d3.select(this).attr('class').match("b1")){
                return "24px";
             } else {
                return "20px";
             }
       });
        

       //調深bar
        svg.selectAll('#'+id)
           .filter('.get_bar')
           .style('fill-opacity',1);
       
};


//define xaxis mouse out   
var xaxis_mout = function(d,i){
   var inside =   d3.select(this)
                    .transition()
                    .duration(300);
   var id = d3.select(this)
              .selectAll('text')
              .attr('id');
    inside.selectAll("rect")
          .style("fill","transparent")
          .style("fill-opacity",1);
    inside.selectAll("text")
          .style("fill","black");
    inside.style("cursor", "default");
   //調回文字
    svg.selectAll('#'+id)
       .filter('.get_bar_t')
       .transition()
       .duration(300)
       .style('font-size',function(d){
          if(d3.select(this).attr('class').match("b1")){
                return "18px";
             } else {
                return "14px";
             }
       });
   //調回bar
        svg.selectAll('#'+id)
           .filter('.get_bar')
           .transition()
           .duration(300)
           .style('fill-opacity',0.7);
};

//xaxis text
var xaxis = svg.selectAll(".xaxis")
               .data(data_location_table)
               .enter()
               .append("g")
               .attr("id","g_xaxis");
//rect filter set
svg.append("defs")
     .append("filter")
     .attr("id","gaussian")
     .attr("x", 0) 
     .attr("y", 0) 
     .append("feGaussianBlur")
     .attr("in","SourceGraphic")
     .attr("stdDeviation","1");




xaxis.append('rect')
     .attr('id',function(d){
         return d.location_w;})
     .attr('height',x_height-10)
     .attr('width',barwidth - space)
     .attr('x', function(d,i){
           return padding.left + i * barwidth ;
        })
     .attr('y', bar_height+5)
     .style('fill','transparent')
     .style('stroke','black')
     .style('stroke-width','2px')
     .style("filter","url(#gaussian)")
     .style("fill-opacity",1);

xaxis.append('text')
     .attr('id',function(d){
         return d.location_w;})
     .style('fill','black')
     .style('font-family','Microsoft JhengHei')
     .style('font-weight','bold')
     .attr('text-anchor','middle')    
     .attr('font-size','23px')
     .attr('x', function(d,i){
           return padding.left + i * barwidth ;
        })
     .attr('y', bar_height)
     .attr('dx',(barwidth - space)/2)
     .attr('dy','1.2em')
     .text(function(d){
        return d.location;
       });
       
svg.selectAll("#g_xaxis")
   .on("mouseenter",xaxis_mover)
   .on("mouseleave",xaxis_mout);
