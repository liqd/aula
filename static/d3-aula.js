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
                .attr("class", "btn")
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

        var rootElem = d3.select(rootSel).append("header").attr("class", "delagation-header");
        // rootElem.append("label").text("Geltungsbereich auswählen");
        var menuDiv = rootElem.append("div");
        var buttonDiv = rootElem.append("div").attr("class", "button-group");
        buttonDiv.append("input")
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
            var wallElasticity = 10;
            force.nodes().forEach(function(n) {
                if (n.x < 0)      n.x = wallElasticity;
                if (n.x > globalGraphWidth)  n.x = globalGraphWidth - wallElasticity;
                if (n.y < 0)      n.y = wallElasticity;
                if (n.y > globalGraphHeight) n.y = globalGraphHeight - wallElasticity;
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
            var dr = Math.sqrt(dx * dx + dy * dy);
            return "M" + d.source.x + "," + d.source.y + "A" + dr + "," + dr + " 0 0,1 " + d.target.x + "," + d.target.y;
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

            svg.selectAll("g path").data([]).exit().remove();
            svg.selectAll("g image").data([]).exit().remove();
            svg.selectAll("g text").data([]).exit().remove();

            path = svg.append("g")
                .selectAll("path").data(force.links())
                .enter().append("path")
                .attr("class", function(d) { return "link default"; })
                .attr("marker-end", function(d) { return "url(#default)"; });

            avat = svg.append("g")
                .selectAll("image").data(force.nodes())
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
                .attr("class", function(d) { return setvisibility(false, this); })
                .text(function(d) { return (d.name + " [" + d.power + "]"); });

            force.alpha(.3);
        };

        var updateWidgetJustTitles = function() {
            // there is still something wrong with updateWidget that
            // destroys the state if we call it from here, so we'll do
            // something simpler.

            text.attr("class", function(d) { return setvisibility(d.showTitle, this); });
        };

        var avatarWidthHeight = function(d) {
            return 20 + Math.min(Math.sqrt(d.power * 100), 160);
        };

        var avatarXPos = function(d) {
            return d.x - (avatarWidthHeight(d) / 2);
        };

        var avatarYPos = function(d) {
            return d.y - (avatarWidthHeight(d) / 2);
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
                        if (newVisibilityStatus === undefined) {
                            newVisibilityStatus = !visible(l.source);
                        }
                        visible(l.source) = newVisibilityStatus;
                        traverse(l.source);
                    }
                });
            };

            traverse(delegate);
            updateVisibility();
        };

        // not sure we should use dblclick.  doesn't seem to work very
        // well in firefox, and not sure about phones, either.
        var on_dblclick = function(d) {
        };

        var on_mouseover = function(d) {
            d.showTitle = true;
            updateWidgetJustTitles();
            d.fixed = true;
        };

        var on_mouseout = function(d) {
            d.showTitle = false;
            updateWidgetJustTitles();
            d.fixed = false;
        };


        // [initialization]

        // tweak hints: width should depend on browser width; height
        // should depend on total voting power of all nodes in scope.
        var globalGraphWidth = 600;
        var globalGraphHeight = 600;

        graph.nodes.forEach(function(d) {
            d.visibleByPower = true;
            d.visibleByMatching = true;
        });

        var visible = function(d) {
            return d.visibleByPower && d.visibleByMatching;
        }

        var force = d3.layout.force()
            .size([globalGraphWidth, globalGraphHeight])
            .nodes(graph.nodes)
            .links(graph.links)
            .on("tick", tick)
            .charge(-200)
            .linkDistance(70)
            .start();

        initializeControlPanel(rootSel, filterByPower, filterMatching);

        var svg = d3.select("div#aula-d3-view")
            .append("div")
               .classed("svg-container", true) // container class to make it responsive
               .append("svg")
               // responsive SVG needs these 2 attributes and no width and height attr
               .attr("preserveAspectRatio", "xMinYMin meet")
               .attr("viewBox", function() { return "0 0 " + globalGraphWidth + " " + globalGraphHeight; })
               // class to make it responsive
               .classed("svg-content-responsive", true);


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

        updateWidget();
    };

    var initializeControlPanel = function(rootSel, filterByPower, filterMatching) {
        var controls = d3.select(".delagation-header").append("div").attr("class", "controls");
        var defaultPowerValue = 2;
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
        showNavigation(".aula-d3-navig", aulaDScopeCurrent, aulaDScopeTree);
        if (d3.selectAll(".aula-d3-navig").length > 0) {
            showGraph(".aula-d3-view", aulaDelegationData);
        }
    };

})();
