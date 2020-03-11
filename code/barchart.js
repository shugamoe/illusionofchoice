// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)

height = 2 * height
var barHeight = Math.floor(height / data.length);

bar = svg.appendMany('g', data)
         .translate((d, i) => [0, i * barHeight]);

bar.appendMany('rect', data)
   .attr('width', d => .8 * width)
   .attr('height', barHeight)
   .attr('y', (d, i) => i * barHeight)
   .attr('fill', 'steelblue');

bar.append("text")
  .attr("x", 0 )
  .attr("y", barHeight / 2)
  .attr("dy", ".35em")
  .text(d => d.brandName);

// d3-jetpack derp level fiddling
// svg.selectAll('rect')
   // .data(data)
   // .enter().append('rect')//.appendMany('rect', data)
     // .at((d, i) => {width: .8 * width,
                    // height: barHeight,
                    // y: i * barHeight,
                    // fill: 'steelblue'
  // });
//
// d3.select("body").append("div")
    // .attrs(function(d, i) { return {title: d.title, id: "id-" + i}; });
