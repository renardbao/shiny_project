// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

svg.selectAll('text')
   .data(data)
   .enter()
   .filter(function(d){
      if(d.variable.match('get_num'))
         return true;
      else
         return false;
   })
   .append('text')
   .attr('x', 10)
   .attr('y', function(d,i){
     return i * 30;
   })
   .text(function(d){
     return d.variable;
   });

//哈哈