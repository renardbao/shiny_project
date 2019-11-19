var padding = {
  top : 10,
  right : 20,
  bottom : 10,
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
                     return d.value}) + padding.top])
                  .range([0,bar_height]);

var data_length = d3.map(data,function(d){
                     return d.location})
                     .keys()
                     .length;


var barwidth = Math.ceil((width-padding.right - padding.left)/data_length);
//bar條間的空隙

var space = 5;

//define bar
var bar = svg.selectAll('#get_bar')
   .select("svg")
   .data(data)
   .enter()
   .filter(function(d){
      if(d.variable.match('get_num'))
         return true;
      else
         return false;
   });



       



//bar
bar.append('rect')
   .attr('fill',function(d){
       if(d.variable == 'get_num'){
          return 'steelblue';
       } else if(d.variable == 'get_num_m'){
          return 'yellow';
       } else{
          return 'orange';
       }
   }
   
  )
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
   .style('fill','black')
   .style('font-family','Microsoft JhengHei')
   .style('font-weight','bold')
   .attr('text-anchor','middle')    
   .attr('font-size',function(d){
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

bar.append('text')
     .style('fill','black')
     .style('font-family','Microsoft JhengHei')
     .style('font-weight','bold')
     .style('writing-mode','tb')
     .attr('x',padding.left/2)
     .attr('y',bar_height/2)
     .text('已接待');
              

