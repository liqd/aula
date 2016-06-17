// inspiring examples:
// http://bl.ocks.org/mbostock/1153292
// http://bl.ocks.org/mbostock/2706022

var aulaDelegationMain = function(graph) {
    var width = 960;
    var height = 800;

    var tick = function() {
        link.attr("x1", function(d) { return d.source.x; })
            .attr("y1", function(d) { return d.source.y; })
            .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; });

        node.attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; });

        text.attr("dx", function(d) { return d.x; })
            .attr("dy", function(d) { return d.y; });

        avat.attr("x", function(d) { return d.x; })
            .attr("y", function(d) { return d.y; });
    };

    function linkArc(d) {
        var dx = d.target.x - d.source.x;
        var dy = d.target.y - d.source.y;
        var dr = Math.sqrt(dx * dx + dy * dy);
        return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
    };

    var force = d3.layout.force()
        .charge(-180)
        .linkDistance(70)
        .size([width, height])
        .on("tick", tick);

    force
        .nodes(graph.nodes)
        .links(graph.links)
        .start();

    var svg = d3.select(".d3_aula").append("svg")
        .attr("width", width)
        .attr("height", height);

    var link = svg
        .selectAll(".link")
        .data(graph.links).enter()
        .append("line")
        .attr("class", "link");

    var node = svg.append("g")
        .selectAll(".node")
        .data(graph.nodes).enter()
        .append("circle")
        .attr("class", "node")
        .attr("r", function(d) { return (20 + 3 * d.power); })
        .call(force.drag);

    var text = svg.append("g")
        .selectAll("text")
        .data(graph.nodes).enter()
        .append("text")
        .attr("dx", ".10em")
        .attr("dy", ".10em")
        .text(function(d) { return d.name; });

    var avat = svg.append("g")
        .selectAll("image")
        .data(graph.nodes).enter()
        .append("image")
        .attr("x", ".10em")
        .attr("y", ".10em")
        .attr("width", "1cm")
        .attr("height", "1cm")
        .attr("xlink:href", function(d) { if (d.avatar) debugger; return d.avatar; });

    /*
        http://stackoverflow.com/questions/13691463/svg-how-to-crop-an-image-to-a-circle
        <svg xmlns="http://www.w3.org/2000/svg" width="100%" height="100%">
          <clipPath id="clipCircle">
            <circle r="50" cx="50" cy="50"/>
          </clipPath>
          <rect width="100" height="100" clip-path="url(#clipCircle)"/>
        </svg>
    */

};

window.onload = function() { aulaDelegationMain(aulaDelegationData); };
