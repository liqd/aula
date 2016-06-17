(function() {

    //////////////////////////////////////////////////////////////////////

    // dictionary that maps dscopes to their sibling trees and
    // ancestor paths.  (the siblings attribute needs to be lazified
    // by lambda abstraction because the right siblings do not exist
    // yet when it is constructed.)
    var buildDScopeTreeIndex = function(aulaDScopeTree) {
        var treeix = {};

        var f = function(tree, ancestors) {
            var parent = ancestors[ancestors.length - 1];
            ancestors.push(tree.dscope);
            treeix[tree.dscope] = {
                "ancestors": ancestors.slice(),
                "subtree": tree,
                "siblings": function() {
                    return !parent
                        ? []
                        : treeix[parent].subtree.children.map(function(c) {
                            return treeix[c.dscope].subtree;
                        });
                },
            }
            tree.children.map(function(d) { f(d, ancestors); });
            ancestors.pop();
        };

        f(aulaDScopeTree, []);
        return treeix;
    };


    //////////////////////////////////////////////////////////////////////

    var showNavigation = function(rootSel, current, tree) {
        var treeix = buildDScopeTreeIndex(tree);

        var update = function() {
            updateMenus();
            updateButtons();
        };

        var updateMenus = function() {
            var mkSelects = function(ancestors) {
                var result = [];
                for (i in ancestors) {
                    if (ancestors[i]) {
                        result.push(treeix[ancestors[i]]);
                    }
                }
                return result;
            }

            var mkSelected = function(d) {
                return treeix[current].ancestors.indexOf(d.dscope) >= 0
                    ? true
                    : undefined;  // 'undefined' is the only thing that works here!
            }

            var select = menuDiv
                .selectAll("select").data(mkSelects(treeix[current].ancestors));
            select.exit()
                .remove();
            select.enter()
                .append("select")
                .attr("name", function(d) { return d.ancestors[d.ancestors.length - 1]; })
                .on("change", function(d) { current = this.value; update(); });

            select
                .selectAll("option").data(function(d) { return d.siblings(); }).enter()
                .append("option")
                .attr("value", function(d) { return d.dscope; })
                .attr("selected", mkSelected)
                .text(function(d) { return d.text; });
        };

        var updateButtons = function() {
            var button = buttonDiv
                .selectAll("button")
                .data(["moreLevels", "fewerLevels"]);
            button.enter()
                .append("button")
                .text(function(d) {
                    if (d == "moreLevels") {
                        return "aufklappen";
                    } else if (d == "fewerLevels") {
                        return "zuklappen";
                    }
                })
                .on("click", function(d) {
                    if (d == "moreLevels") {
                        current = treeix[current].subtree.children[0].dscope;
                    } else if (d == "fewerLevels") {
                        var ancs = treeix[current].ancestors;
                        current = ancs[ancs.length - 2];
                    }
                    update();
                });
            button
                .attr("disabled", function(d) {
                    if (d == "moreLevels") {
                        return treeix[current].subtree.children.length == 0 || undefined;
                    } else if (d == "fewerLevels") {
                        return treeix[current].ancestors.length == 1 || undefined;
                    }
                })
        };

        var rootElem = d3.select(rootSel).append("div");
        rootElem.append("label").text("Geltungsbereich auswählen");
        var menuDiv = rootElem.append("div");
        var buttonDiv = rootElem.append("div");
        rootElem.append("input")
            .attr("value", "anzeigen")
            .attr("type", "submit")
            .on("click", function() { document.location.href = "/delegation/view?scope=" + current; });

        update();
    };


    //////////////////////////////////////////////////////////////////////

    var showGraph = function(rootSel, graph) {
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

        var svg = d3.select(rootSel).append("svg")
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


    //////////////////////////////////////////////////////////////////////

    window.onload = function() {
        showNavigation(".aula-d3-navig", aulaDScopeCurrent, aulaDScopeTree);
        showGraph(".aula-d3-view", aulaDelegationData);
    };

})();
