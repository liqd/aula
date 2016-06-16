// inspirating example:
// http://bl.ocks.org/mbostock/1153292

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
        .data(graph.links)
        .enter()
        .append("line")
        .attr("class", "link");

    var node = svg.append("g")
        .selectAll(".node")
        .data(graph.nodes)
        .enter()
        .append("circle")
        .attr("class", "node")
        .attr("r", function(d) { return (20 + 3 * d.power); })
        .call(force.drag);

    var text = svg.append("g")
        .selectAll("text")
        .data(graph.nodes)
        .enter()
        .append("text")
        .attr("dx", ".10em")
        .attr("dy", ".10em")
        .text(function(d) { return d.name; });

/*
    node.append("svg:image")
        .attr("xlink:href", function(d) {
            return "http://zierfischverzeichnis.de/klassen/pisces/perciformes/percoidei/thumbnails/gymnocephalus_cernuus.gif";
        });
*/



};

window.onload = function() { aulaDelegationMain(aulaDelegationData); };


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

-- more FIXMEs:
- what you click moves to top (covers other nodes)
- make bounds strong (no nodes may escape)

*/
