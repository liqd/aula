(function() {

    //////////////////////////////////////////////////////////////////////

    // dictionary that maps dscopes to their sibling trees and
    // ancestor paths.  (the siblings attribute needs to be lazified
    // by lambda abstraction because the right siblings do not exist
    // yet when it is constructed.)

    var buildDScopeIndex = function(forest) {
        var dscopeix = {};

        var f = function(tree, ancestors) {
            var parent = ancestors[ancestors.length - 1];
            ancestors.push(tree.dscope);
            dscopeix[tree.dscope] = {
                "ancestors": ancestors.slice(),
                "subtree": tree,
                "siblings": function() {
                    var sibs = !parent ? forest : dscopeix[parent].subtree.children;
                    var options = sibs.map(function(c) {
                        return dscopeix[c.dscope].subtree;
                    });
                    // prepend the '*' entry to every menu except the top
                    if (parent) {
                        var alloption = {
                            "children": dscopeix[parent].subtree.children,
                            "dscope": dscopeix[parent].subtree.dscope,
                            "text": "*"
                        };
                        options.unshift(alloption);
                    }
                    return options;
                },
            }
            tree.children.map(function(d) { f(d, ancestors); });
            ancestors.pop();
        };

        forest.map(function(tree) { f(tree, []); });
        return dscopeix;
    };


    //////////////////////////////////////////////////////////////////////

    var showNavigation = function(rootSel, current, forest) {
        var dscopeix = buildDScopeIndex(forest);

        var update = function() {
            var mkSelects = function(ancestors) {
                var result = [];
                for (i in ancestors) {
                    if (ancestors[i]) {
                        result.push(dscopeix[ancestors[i]]);
                    }
                }
                // where can we go down from the current dscope?
                var drillDownOptions = dscopeix[ancestors[i]].subtree.children;
                if (drillDownOptions.length > 0) {
                    // (the dscopeix has been constructed with a '*'
                    // option that points to the parent dscope.)
                    result.push(dscopeix[drillDownOptions[0].dscope]);
                }
                return result;
            };

            var mkSelected = function(d) {
                return dscopeix[current].ancestors.indexOf(d.dscope) >= 0
                    ? true
                    : undefined;  // 'undefined' is the only thing that works here!
            };

            var select = menuDiv
                .selectAll("select").data(mkSelects(dscopeix[current].ancestors));
            select.exit()
                .remove();

            // create <select> elems
            select.enter()
                .append("select")
                .attr("name", function(d) { return d.ancestors[d.ancestors.length - 1]; })
                .on("change", function(d) { document.location.href = "/delegation/view?scope=" + this.value; });

            // create <option> elems
            select
                .selectAll("option").data(function(d) { return d.siblings(); })
                .enter()
                .append("option")
                .attr("value", function(d) { return d.dscope; })
                .attr("selected", mkSelected)
                .text(function(d) { return d.text; });
        };

        var rootElem = d3.select(rootSel).append("header").attr("class", "delagation-header");
        rootElem.append("h2").attr("class", "sub-heading")
            .text("Ausgewählt: " + dscopeix[current].subtree.text);
        var menuDiv = rootElem.append("div");
        var buttonDiv = rootElem.append("div").attr("class", "button-group");
        update();
    };


    //////////////////////////////////////////////////////////////////////

    var showGraph = function(rootSel, current, graph) {

        // [local functions]

        var tick = function() {
            // adjust positions (is there a better place for this than here in the tick function?)
            var wallElasticity = 5;
            force.nodes().forEach(function(n) {
                if (n.x < 0)                n.x = wallElasticity;
                if (n.x > globalGraphWidth) n.x = globalGraphWidth - wallElasticity;
                if (n.y < 0)                n.y = wallElasticity;
                if (n.y > globalGraphWidth) n.y = globalGraphWidth - wallElasticity;
            });

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
            var dr = Math.max(1, Math.sqrt(dx * dx + dy * dy));  // arrow length
            var stretchFactorS = (dr - avatarRadius(d.source)) / dr;
            var stretchFactorT = (dr - avatarRadius(d.target)) / dr;
            var startx = d.target.x - (dx * stretchFactorS);
            var starty = d.target.y - (dy * stretchFactorS);
            var endx   = d.source.x + (dx * stretchFactorT);
            var endy   = d.source.y + (dy * stretchFactorT);
            return "M" + startx + "," + starty + "L" + endx + "," + endy;
        };

        // make all nodes below a certain power threshold invisible.
        var filterByPower = function(threshold) {
            graph.nodes.forEach(function(n) {
                n.visibleByPower = n.power >= threshold;
            });
            updateVisibility();
        };

        var filterMatching = function(substring) {
            graph.nodes.forEach(function(n) {
                n.visibleByMatching = substring === "" || n.name.indexOf(substring) >= 0;
                n.showTitleMatching = substring !== "" && n.name.indexOf(substring) >= 0;
            });
            updateVisibility();
        };

        var updateVisibility = function() {
            var gnodes = [];
            var glinks = [];

            // we need to use `graph` here, not `force`.  invisible
            // nodes are still in the former, but not in the latter.
            graph.nodes.forEach(function(n) {
                if (visible(n)) {
                    gnodes.push(n);
                }
            });

            graph.links.forEach(function(l) {
                if (visible(l.source) && visible(l.target)) {
                    glinks.push(l);
                }
            });

            force.nodes(gnodes).links(glinks);
            updateWidget();
        };

        // this is called if the set of nodes changes (i.e., if nodes
        // are removed or re-added).
        var updateWidget = function() {
            // (we remove all nodes from the svg and then add them
            // again.  the `.update` method could offer some tuning
            // potential, should we experience low frame rates.  or it
            // may not be the bottleneck, who knows.)

            container.selectAll("g path").data([]).exit().remove();
            container.selectAll("g image").data([]).exit().remove();
            container.selectAll("g text").data([]).exit().remove();

            if (force.nodes().length > 0) {
                avatarWidthHeight.refresh();
                force.nodes().forEach(function(n) {
                    n.fixed = false;  // (otherwise nodes sometimes
                                      // get away from under the mouse
                                      // without triggering the
                                      // event.)
                });

                path = container.append("g")
                    .selectAll("path").data(force.links())
                    .enter().append("path")
                    .attr("class", function(d) { return "link default" + (d.dscope === current ? "" : " implicit"); })
                    .attr("marker-end", function(d) { return "url(#default)"; });

                avat = container.append("g")
                    .selectAll("image").data(force.nodes())
                    .enter().append("image")
                    .attr("class", ".node")
                    .call(force.drag)
                    .attr("width",  avatarWidthHeight.get)
                    .attr("height", avatarWidthHeight.get)
                    .attr("xlink:href", function(d) { return d.avatar; });

                avat.on("click",      on_click)
                    .on("dblclick",   on_dblclick)
                    .on("mouseover",  on_mouseover)
                    .on("mouseout",   on_mouseout);

                text = container.append("g")
                    .selectAll("text").data(force.nodes())
                    .enter().append("text")
                    .attr("class", function(d) { return setvisibility(visibleTitle(d), this); })
                    .text(function(d) { return (d.name + " [" + d.power + "]"); });

                force.alpha(.3);
            }

            zoom();
        };

        var zoom = function() {
            var calcGraphWidth = function(numNodes) {
                var nodesPerTile = 50;
                var tileWidth = 600;
                return 100 + Math.sqrt(numNodes / nodesPerTile) * tileWidth;
            };

            globalGraphWidth = calcGraphWidth(force.nodes().length);
            force.size([globalGraphWidth, globalGraphWidth]);
            svg.attr("viewBox", "0 0 " + globalGraphWidth + " " + globalGraphWidth);
        };

        var updateWidgetJustTitles = function() {
            // there is still something wrong with updateWidget that
            // destroys the state if we call it from here, so we'll do
            // something simpler.

            text.attr("class", function(d) { return setvisibility(visibleTitle(d), this); });
        };

        var avatarWidthHeight = (function() {
            var refresh = function() {
                var flat = force.nodes().slice()
                                .sort(function(n, m) { return n.power - m.power; });
                var groups = [];
                flat.forEach(function(n) {
                    var lastGroup = groups[groups.length - 1]
                    if (lastGroup && lastGroup[0].power === n.power) {
                        lastGroup.push(n);
                    } else {
                        groups.push([n]);
                    }
                });

                var low = 15;
                var high = force.linkDistance() - 10;
                var step = (high - low) / groups.length;
                var cursor = low;

                groups.forEach(function(g) {
                    g.forEach(function(n) {
                        n.powerInPixels = cursor;
                    });
                    cursor += step;
                });
            };

            var get = function(d) {
                if (!d.powerInPixels) {
                    refresh();
                }
                return d.powerInPixels;
            };

            return { refresh: refresh, get: get };
        })();

        var avatarRadius = function(d) {
            return avatarWidthHeight.get(d) / 2;
        };

        var avatarXPos = function(d) {
            return d.x - (avatarWidthHeight.get(d) / 2);
        };

        var avatarYPos = function(d) {
            return d.y - (avatarWidthHeight.get(d) / 2);
        };

        // toggle visibility of all delegatees (recursively).
        //
        // (currently, if we click around in a large delegatee tree,
        // turning individual sub-trees off and on, and then close and
        // open the entire tree, everything will be visible.  it would
        // be slightly nicer to remember which nodes were invisible
        // and recover the state before the previous click on the root
        // node.)
        var on_click = function(delegate) {
            // the fun arg is the node that is clicked on.  it is
            // called the delegate here because we process its
            // delegatees.
            var newVisibilityStatus = undefined;
            var setNewVisibilityStatus = function(n) {
                if (newVisibilityStatus === undefined) {
                    newVisibilityStatus = !visible(n);
                }
                n.visibleByClick = newVisibilityStatus;
                // clicking overrides the other filters
                if (newVisibilityStatus) {
                    makeAllVisible(n);
                }
            };

            // inbound edges (full depth)
            var visited = [];
            var traverse = function(d) {
                if (visited.indexOf(d.name) >= 0) {
                    return;
                }
                visited.push(d.name);
                graph.links.forEach(function(l) {
                    // the target of the edge needs to be in the
                    // sub-graph, but the source must not be the one
                    // that we just clicked on: we don't want to close
                    // the click-on node, or we won't be able to
                    // re-open it.
                    if (l.target.name === d.name && l.source.name !== delegate.name) {
                        setNewVisibilityStatus(l.source);
                        traverse(l.source);
                    }
                });
            };
            traverse(delegate);

            // outbound edges (only one level of delegates)
            graph.links.forEach(function(l) {
                if (l.source.name === delegate.name) {
                    setNewVisibilityStatus(l.target);
                }
            });

            // commit
            updateVisibility();
        };

        // not sure we should use dblclick.  doesn't seem to work very
        // well in firefox, and not sure about phones, either.
        var on_dblclick = function(d) {
        };

        var on_mouseover = function(d) {
            d.showTitleMouseOver = true;
            updateWidgetJustTitles();
            d.fixed = true;
        };

        var on_mouseout = function(d) {
            d.showTitleMouseOver = false;
            updateWidgetJustTitles();
            d.fixed = false;
        };


        // [initialization]

        // tweak hints: width should depend on browser width; height
        // should depend on total voting power of all nodes in scope.
        var globalGraphWidth = 600;

        var visible = function(d) {
            return d.visibleByPower && d.visibleByMatching && d.visibleByClick;
        };

        var visibleTitle = function(d) {
            return d.showTitleMatching || d.showTitleMouseOver;
        };

        var makeAllVisible = function(d) {
            d.visibleByPower = true;
            d.visibleByMatching = true;
            d.visibleByClick = true;
        };

        graph.nodes.forEach(makeAllVisible);

        var force = d3.layout.force()
            .size([globalGraphWidth, globalGraphWidth])
            .nodes(graph.nodes)
            .links(graph.links)
            .on("tick", tick)
            .charge(-200)
            .linkDistance(70);

        initializeControlPanel(rootSel, filterByPower, filterMatching);

        var svg = d3.select("div#aula-d3-view")
            .append("div")
               .classed("svg-container", true) // container class to make it responsive
               .append("svg")
               // responsive SVG needs these 2 attributes and no width and height attr
               .attr("preserveAspectRatio", "xMinYMin meet")
               .attr("viewBox", "0 0 " + globalGraphWidth + " " + globalGraphWidth)
               // class to make it responsive
               .classed("svg-content-responsive", true);


        svg.append("defs")
            .selectAll("marker")
            .data(["default"]).enter().append("marker")
            .attr("id", function(d) { return d; })
            .attr("viewBox", "0 -5 10 10")
            .attr("refX", 10)
            .attr("refY", 0)
            .attr("markerWidth", 6)
            .attr("markerHeight", 6)
            .attr("orient", "auto")
            .append("path")
            .attr("d", "M0,-5L10,0L0,5");

        var container = svg.append("g");

        var path = undefined;
        var avat = undefined;
        var text = undefined;

        updateWidget();
        force.start();
    };

    var initializeControlPanel = function(rootSel, filterByPower, filterMatching) {
        var controls = d3.select(".delagation-header").append("div").attr("class", "controls");
        var defaultPowerValue = 1;
        setTimeout(function() { filterByPower(defaultPowerValue); });

        var ig1 = controls.append("div").attr("class", "input-group");
        ig1.append("label").text("Untergrenze Anzahl Beauftragungen:");
        ig1.append("input")
            .attr("type", "number")
            .attr("class", "input-text input-number")
            .attr("value", defaultPowerValue)
            .attr("min", 1)
            .on("keyup",   function() { filterByPower(this.value); })
            .on("mouseup", function() { filterByPower(this.value); });

        var ig2 = controls.append("div").attr("class", "input-group");
        ig2.append("label").text("Nutzer suchen:");
        ig2.append("input")
            .attr("type", "text")
            .attr("class", "input-text")
            .on("keyup",   function() { filterMatching(this.value); })
            .on("mouseup", function() { filterMatching(this.value); });
    };

    // FIXME: i think d3js has a better way to do this.
    var setvisibility = function(visible, elem) {
        var result = "";
        if (elem.attributes['class']) {
            result = elem.attributes['class'].value;
        }

        // remove hidden class
        result = result.replace(" hidden", "");
        result = result.replace("hidden", "");

        // add it if appropriate
        if (!visible) {
            result = result + " " + "hidden";
        }
        return result;
    };


    //////////////////////////////////////////////////////////////////////

    window.onload = function() {
        showNavigation(".aula-d3-navig", aulaDScopeCurrent, aulaDScopeForest);
        if (d3.selectAll(".aula-d3-navig").length > 0) {
            showGraph(".aula-d3-view", aulaDScopeCurrent, aulaDelegationData);
        }
    };

})();
