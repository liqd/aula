var main = function() {

var width = 960;
var height = 500;

var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);

var svg = d3.select("#d3").append("svg")
    .attr("width", width)
    .attr("height", height);

d3.json("/static/d3-aula-sample-data2.json", function(error, graph) {
  if (error) throw error;

  force
      .nodes(graph.nodes)
      .links(graph.links)
      .start();

  var link = svg.selectAll(".link")
      .data(graph.links)
    .enter().append("line")
      .attr("class", "link");

  var node = svg.selectAll(".node")
      .data(graph.nodes)
    .enter().append("circle")
      .attr("class", "node")
      .attr("r", function(d) { return (20 + 3 * d.power); })
      .call(force.drag);

  node.append("title")
      .text(function(d) { return d.name; });

  node.append("text")
      .attr("dy", ".3em")
      .style("text-anchor", "middle")
      .text(function(d) { return d.name; });

/*
  node.append("svg:image")
        .attr("xlink:href",
                function(d) { return "http://zierfischverzeichnis.de/klassen/pisces/perciformes/percoidei/thumbnails/gymnocephalus_cernuus.gif"; });
*/


  force.on("tick", function() {
    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
  });
});

};

window.onload = main;


/*

        svg_node_groups.append("svg:image")
            .attr("class", function(d) { return d.csscls.join(" "); })
            .attr("xlink:href", function(d) {
                function f(csscls) {
                    var imagePath = image_path + "default.gif";
                    // (if no relevant css class assignments are found, return path to default gif.)

                    if (csscls.hasOwnProperty("length")) {
                        csscls.forEach(function(cls) {
                            switch(cls) {
                            case "bw_bexquery":            imagePath = image_path + "bexquery.gif";            break;

-- FIXME: show avatars
-- FIXME: make context selectable
-- FIXME: double click on node: remove from graph
-- FIXME: only list with hidden nodes, no list ofr shown nodes.  double-click there to show
-- FIXME: show all / hide all

*/
