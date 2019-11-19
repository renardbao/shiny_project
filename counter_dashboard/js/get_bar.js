var padding = {
  top : 20,
  right : 20,
  bottom : 20,
  left : 20
};
var data_length = data.filter(function(d){
      if(d.variable == 'get_num' || d.variable == 'scan')
         return true;
      else
         return false;
   })
   .length;

var barwidth = Math.ceil((width-padding.right - padding.left)/(data_length/2));
var space = 5;


svg.selectAll('rect')
   .data(data)
   .enter()
   .filter(function(d){
      if(d.variable == 'get_num' || d.variable == 'scan')
         return true;
      else
         return false;
   })
   .append('rect')
   .attr('fill', function(d){
       if(d.variable == 'get_num'){
          return 'steelblue';
       } else if( d.variable == 'scan'){
          return 'pink';
       }
   })
   .attr('height', function(d){return d.value;})
   .attr('width', function(d) { return barwidth - space; })
   .attr('x', function(d,i){ 
      if(d.variable == 'get_num')
         return 
      
     return padding.left + i * barwidth ;})
   .attr('y', function(d) { 
      if(d.variable == 'get_num'){
         return height - padding.bottom - d.value ; 
      } else if( d.variable == 'scan')
     return height - padding.bottom - d.value - 100 ; });
 


