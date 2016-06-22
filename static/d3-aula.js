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
                .selectAll("option").data(function(d) {
                    if (d.siblings().length > 0) {
                        return d.siblings();
                    } else {
                        // this is a bit fake; we don't really have a
                        // menu here, there is only one "everywhere".
                        // but symmetry-wise this works well.
                        return [{ "dscope": "global", "text": "In allen Ideenräumen" }];
                    }
                }).enter()
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

        // [local functions]

        var tick = function() {
            // adjust positions (is there a better place for this than here in the tick function?)
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

            text.attr("dx", function(d) { return d.x; })
                .attr("dy", function(d) { return d.y; });

            avat.attr("x", avatarXPos)
                .attr("y", avatarYPos);
        };

        var linkArc = function(d) {
            var dx = d.target.x - d.source.x;
            var dy = d.target.y - d.source.y;
            var dr = Math.sqrt(dx * dx + dy * dy);
            return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
        };

        var updateVisibility = function() {
            var gnodes = [];
            var glinks = [];

            graph.nodes.forEach(function(n) {
                if (n.visible) {
                    gnodes.push(n);
                }
            });

            graph.links.forEach(function(l) {
                if (l.source.visible && l.target.visible) {
                    glinks.push(l);
                }
            });

            force.nodes(gnodes).links(glinks);
            updateWidget();
        };

        var avatarWidthHeight = function(d) {
            // tweak hints: 30, 200 are good bounds for this.
            return (50 + 30 * d.power) / 4;
        };

        var avatarXPos = function(d) {
            return d.x - (avatarWidthHeight(d) / 2);
        };

        var avatarYPos = function(d) {
            return d.y - (avatarWidthHeight(d) / 2);
        };

        var on_click = function(d) {
            d.visible = false;
            updateVisibility();
        };

        var on_dblclick = function(d) {
        };

        var on_mouseover = function(d) {
        };

        var on_mouseout = function(d) {
        };


        // [initialization]

        // tweak hints: width should depend on browser width; height
        // should depend on total voting power of all nodes in scope.
        var width = 960;
        var height = 800;

        graph.nodes.forEach(function(d) {
            d.visible = true;
        });

        var force = d3.layout.force()
            .size([width, height])
            .nodes(graph.nodes)
            .links(graph.links)
            .on("tick", tick)
            .charge(-200)
            .linkDistance(70)
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

        var path = undefined;
        var avat = undefined;
        var text = undefined;

        var updateWidget = function() {
            path = svg.append("g")
                .selectAll("path").data(force.links())
                .enter().append("path")
                .attr("class", function(d) { return "link default"; })
                .attr("marker-end", function(d) { return "url(#default)"; });

            avat = svg.append("g")
                .selectAll(".node").data(force.nodes())
                .enter().append("image")
                .attr("class", ".node")
                .call(force.drag)
                .attr("width",  avatarWidthHeight)
                .attr("height", avatarWidthHeight)
                .attr("xlink:href", function(d) { return d.avatar; });

            avat.on("click",      on_click)
                .on("dblclick",   on_dblclick)
                .on("mouseover",  on_mouseover)
                .on("mouseout",   on_mouseout);

            text = svg.append("g")
                .selectAll("text").data(force.nodes())
                .enter().append("text")
                .text(function(d) { return (d.name + " [" + d.power + "]"); });

            // TODO: .exit().remove();

        };

        updateWidget();
        debugger;
    };


    //////////////////////////////////////////////////////////////////////

    window.onload = function() {
        showNavigation(".aula-d3-navig", aulaDScopeCurrent, aulaDScopeTree);
        if (d3.selectAll(".aula-d3-navig").length > 0) {
            showGraph(".aula-d3-view", aulaDelegationData);
        }
    };

})();
