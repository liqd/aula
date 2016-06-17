(function() {

    var showNavigation = function(aulaDScopeCurrent, aulaDScopeTree) {
        console.log(aulaDScopeCurrent);
        console.log(aulaDScopeTree);
    };

    var showGraph = function(graph) {
        var width = 960;
        var height = 800;

        var tick = function() {
            // adjust positions (FIXME: is there a better place for this than here in the tick function?)
            for (i in graph.nodes) {
                var wallElasticity = 10;
                if (graph.nodes[i]) {
                    if (graph.nodes[i].x < 0)      graph.nodes[i].x = wallElasticity;
                    if (graph.nodes[i].x > width)  graph.nodes[i].x = width - wallElasticity;
                    if (graph.nodes[i].y < 0)      graph.nodes[i].y = wallElasticity;
                    if (graph.nodes[i].y > height) graph.nodes[i].y = height - wallElasticity;
                }
            }

            // update elems
            path.attr("d", linkArc);

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
            .charge(-200)
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

        svg.append("defs")
            .selectAll("marker")
            .data(["default"]).enter().append("marker")
            .attr("id", function(d) { return d; })
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 15)
            .attr("refY", -1.5)
            .attr("markerWidth", 6)
            .attr("markerHeight", 6)
            .attr("orient", "auto")
            .append("path")
            .attr("d", "M0,-5L10,0L0,5");

        var path = svg.append("g")
            .selectAll("path")
            .data(force.links()).enter().append("path")
            .attr("class", function(d) { return "link default"; })
            .attr("marker-end", function(d) { return "url(#default)"; });

        var node = svg.append("g")
            .selectAll(".node")
            .data(graph.nodes).enter().append("circle")
            .attr("class", "node")
            .attr("r", function(d) { return (20 + 3 * d.power); })
            .call(force.drag);

        var text = svg.append("g")
            .selectAll("text")
            .data(graph.nodes).enter().append("text")
            .attr("dx", ".10em")
            .attr("dy", ".10em")
            .text(function(d) { return (d.name + " [" + d.power + "]"); });

        var avat = svg.append("g")
            .selectAll("image")
            .data(graph.nodes).enter().append("image")
            .attr("x", ".10em")
            .attr("y", ".10em")
            .attr("width", "1cm")
            .attr("height", "1cm")
            .attr("xlink:href", function(d) { return d.avatar; });

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

    window.onload = function() {
        showNavigation(aulaDScopeCurrent, aulaDScopeTree);
        showGraph(aulaDelegationData);
    };

})();
