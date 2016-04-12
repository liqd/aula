var width = 960,        // svg width
    height = 600,       // svg height
    dr = 4,             // default point radius
    off = 15,           // cluster hull offset
    expand = {},        // expanded clusters
    data, net, force, force2, hullg, hull, linkg, helper_linkg, link, hlink, nodeg, helper_nodeg, node, hnode,
    debug = 0; // 0: disable, 1: all, 2: only force2

var curve = d3.svg.line()
  .interpolate("cardinal-closed")
  .tension(.85);

var fill = d3.scale.category20();

function noop() { return false; }

function nodeid(n) {
  return n.size > 0 ? "_g_" + n.group + "_" + n.expansion : n.name;
}

function linkid(l) {
  var u = nodeid(l.source),
      v = nodeid(l.target);
  return u<v ? u+"|"+v : v+"|"+u;
}

function getGroup(n) { return n.group; }

function cycleState(d) {
  var g = d.group, s = expand[g] || 0;
  // it's no use 'expanding the intergroup links only' for nodes which only have 1 outside link for real:
  if (d.ig_link_count < 2)
    s = (s ? 0 : 2);
  else {
    s++; s %= 3;
  }
  return expand[g] = s;
}

// constructs the network to visualize
function network(data, prev) {
  expand = expand || {};
  var gm = {},    // group map
      nm = {},    // node map
      nml = {},   // node map for left-side 'link path helper nodes'
      nmr = {},   // node map for right-side 'link path helper nodes'
      nmimg = {}, // node map for cloned nodes for force2
      lm = {},    // link maps - lm ~ lml-lmm-lmr
      lml = {},
      lmm = {},
      lmr = {},
      gn = {},                  // previous group nodes
      gc = {},                  // previous group centroids
      nodes = [],               // output nodes
      links = [],               // output links
      helper_nodes = [],        // helper force graph nodes
      helper_links = [];        // helper force graph links
      helper_render_links = []; // helper force graph links
  var k;

  // process previous nodes for reuse or centroid calculation
  if (prev) {
    prev.nodes.forEach(function(n) {
      var i = getGroup(n), o;
      if (n.size > 0) {
        gn[i] = n;
        n.size = 0;
        n.ig_link_count = 0;
        n.link_count = 0;
        n.first_link = null;
        n.first_link_target = null;
      } else {
        o = gc[i] || (gc[i] = {x:0,y:0,count:0});
        o.x += n.x;
        o.y += n.y;
        o.count += 1; // we count regular nodes here, so .count is a measure for the number of nodes in the group
      }
    });
  }

  // determine nodes
  for (k=0; k<data.nodes.length; ++k) {
    var n = data.nodes[k],
        i = getGroup(n),
        expansion = expand[i] || 0,
        l = gm[i] || (gm[i]=gn[i]) || (gm[i]={group:i, size:0, nodes:[], ig_link_count:0, link_count:0, expansion: expansion}),
        img;

    // we need to create a NEW object when expansion changes from 0->1 for a group node
    // in order to break the references from the d3 selections, so that the next time
    // this group node will indeed land in the 'enter()' set
    if (l.expansion != expansion) {
      l = gn[i] = gm[i] = {group:l.group, x:l.x, y: l.y, size:l.size, nodes:l.nodes, ig_link_count:l.ig_link_count, link_count:l.link_count, expansion: expansion};
    }

    if (expansion == 2) {
      // the node should be directly visible
      nm[nodeid(n)] = n;
      img = {ref: n, x: n.x, y: n.y, size: n.size || 0, fixed: 1, id: nodeid(n)};
      nmimg[nodeid(n)] = img;
      nodes.push(n);
      helper_nodes.push(img);
      if (gn[i]) {
        // place new nodes at cluster location (plus jitter)
        n.x = gn[i].x + Math.random();
        n.y = gn[i].y + Math.random();
      }
    } else {
      // the node is part of a collapsed cluster
      if (l.size == 0) {
        // if new cluster, add to set and position at centroid of leaf nodes
        nm[nodeid(n)] = l;
        l.size = 1;                     // hack to make nodeid() work correctly for the new group node
        nm[nodeid(l)] = l;
        img = {ref: l, x: l.x, y: l.y, size: l.size || 0, fixed: 1, id: nodeid(l)};
        nmimg[nodeid(l)] = img;
        l.size = 0;                     // undo hack
        nmimg[nodeid(n)] = img;
        nodes.push(l);
        helper_nodes.push(img);
        if (gc[i]) {
          l.x = gc[i].x / gc[i].count;
          l.y = gc[i].y / gc[i].count;
        }
      } else {
        // have element node point to group node:
        nm[nodeid(n)] = l; // l = shortcut for: nm[nodeid(l)];
        nmimg[nodeid(n)] = nmimg[nodeid(l)];
      }
      l.nodes.push(n);
    }
    // always count group size as we also use it to tweak the force graph strengths/distances
    l.size += 1;
    n.group_data = l;
    n.link_count = 0;
    n.first_link = null;
    n.first_link_target = null;
  }

  // determine links
  for (k=0; k<data.links.length; ++k) {
    var e = data.links[k],
        u = getGroup(e.source),
        v = getGroup(e.target),
        rui, rvi, ui, vi, lu, rv, ustate, vstate, uimg, vimg,
        i, ix,
        l, ll, l_, lr;
    if (u != v) {
      gm[u].ig_link_count++;
      gm[v].ig_link_count++;
    }
    ustate = expand[u] || 0;
    vstate = expand[v] || 0;
    // while d3.layout.force does convert link.source and link.target NUMERIC values to direct node references,
    // it doesn't for other attributes, such as .real_source, so we do not use indexes in nm[] but direct node
    // references to skip the d3.layout.force implicit links conversion later on and ensure that both .source/.target
    // and .real_source/.real_target are of the same type and pointing at valid nodes.
    rui = nodeid(e.source);
    rvi = nodeid(e.target);
    u = nm[rui];
    v = nm[rvi];
    if (u == v) {
      // skip links from node to same (A-A); they are rendered as 0-length lines anyhow. Less links in array = faster animation.
      continue;
    }
    // 'links' are produced as 3 links+2 helper nodes; this is a generalized approach so we
    // can support multiple links between element nodes and/or groups, always, as each
    // 'original link' gets its own set of 2 helper nodes and thanks to the force layout
    // those helpers will all be in different places, hence the link 'path' for each
    // parallel link will be different.
    ui = nodeid(u);
    vi = nodeid(v);
    i = (ui < vi ? ui+"|"+vi : vi+"|"+ui);
    l = lm[i] || (lm[i] = {source:u, target:v, size:0, distance: 0});
    if (ustate == 1) {
      ui = rui;
    }
    if (vstate == 1) {
      vi = rvi;
    }
    ix = (ui < vi ? ui+"|"+vi+"|"+ustate+"|"+vstate : vi+"|"+ui+"|"+vstate+"|"+ustate);
    ix = (ui < vi ? ui+"|"+vi : vi+"|"+ui);
    // link(u,v) ==> u -> lu -> rv -> v
    lu = nml[ix] || (nml[ix] = data.helpers.left[ix]  || (data.helpers.left[ix]  = {ref: u, id: "_lh_" + ix, size: -1, link_ref: l}));
    rv = nmr[ix] || (nmr[ix] = data.helpers.right[ix] || (data.helpers.right[ix] = {ref: v, id: "_rh_" + ix, size: -1, link_ref: l}));
    uimg = nmimg[ui];
    vimg = nmimg[vi];
    ll = lml[ix] || (lml[ix] = {g_ref: l, ref: e, id: "l"+ix, source:uimg, target:  lu, real_source:u, real_target:v, size:0, distance: 0, left_seg  : true});
    l_ = lmm[ix] || (lmm[ix] = {g_ref: l, ref: e, id: "m"+ix, source:  lu, target:  rv, real_source:u, real_target:v, size:0, distance: 0, middle_seg: true});
    lr = lmr[ix] || (lmr[ix] = {g_ref: l, ref: e, id: "r"+ix, source:  rv, target:vimg, real_source:u, real_target:v, size:0, distance: 0, right_seg : true});
    l.size += 1;
    ll.size += 1;
    l_.size += 1;
    lr.size += 1;

    // these are only useful for single-linked nodes, but we don't care; here we have everything we need at minimum cost.
    if (l.size == 1) {
      u.link_count++;
      v.link_count++;
      u.first_link = l;
      v.first_link = l;
      u.first_link_target = v;
      v.first_link_target = u;
    }
  }

  for (k in lm) { links.push(lm[k]); }
  for (k in lml) { helper_links.push(lml[k]); }
  for (k in lmm) { helper_links.push(lmm[k]); helper_render_links.push(lmm[k]); }
  for (k in lmr) { helper_links.push(lmr[k]); }
  for (k in nml) { helper_nodes.push(nml[k]); }
  for (k in nmr) { helper_nodes.push(nmr[k]); }

  return {nodes: nodes, links: links, helper_nodes: helper_nodes, helper_links: helper_links, helper_render_links: helper_render_links};
}

function convexHulls(nodes, offset) {
  var hulls = {};

  // create point sets
  for (var k=0; k<nodes.length; ++k) {
    var n = nodes[k];
    if (n.size) continue;
    var i = getGroup(n),
        l = hulls[i] || (hulls[i] = []);
    l.push([n.x-offset, n.y-offset]);
    l.push([n.x-offset, n.y+offset]);
    l.push([n.x+offset, n.y-offset]);
    l.push([n.x+offset, n.y+offset]);
  }

  // create convex hulls
  var hullset = [];
  for (i in hulls) {
    hullset.push({group: i, path: d3.geom.hull(hulls[i])});
  }

  return hullset;
}

function drawCluster(d) {
  return curve(d.path); // 0.8
}

// these functions call init(); by declaring them here,
// they don't have the old init() as a closure any more.
// This should save us some memory and cycles when using
// this in a long-running setting.

function on_hull_click(d) {
  if (debug == 1) console.log("node click", d, arguments, this, expand[d.group]);
  // clicking on 'path helper nodes' shouln't expand/collapse the group node:
  if (d.size < 0)
    return;
  cycleState(d);
  init();
}

function on_node_click(d) {
  if (debug == 1) console.log("node click", d, arguments, this, expand[d.group]);
  // clicking on 'path helper nodes' shouln't expand/collapse the group node:
  if (d.size < 0)
    return;
  cycleState(d);
  init();
}

// --------------------------------------------------------

var body = d3.select("#d3");

var vis = body.append("svg")
   .attr("width", width)
   .attr("height", height);


var pathgen = d3.svg.line().interpolate("basis");

d3.json("/static/d3-aula-sample-data-class-with-25-fake-cluster.json", function(json) {
  /*
  JSON layout:

  {
    "nodes": [
      {
        "name"  : "bla",    // in this code, this is expected to be a globally unique string (as it's used for the id via nodeid())
        "group" : 1         // group ID (number)
      },
      ...
    ],
    "links": [
      {
        "source" : 1,       // nodes[] index (number; is immediately converted to direct nodes[index] reference)
        "target" : 0,       // nodes[] index (number; is immediately converted to direct nodes[index] reference)
        "value"  : 1        // [not used in this force layout]
      },
      ...
    ]
  }
  */
  data = json;
  for (var i=0; i<data.links.length; ++i) {
    o = data.links[i];
    o.source = data.nodes[o.source];
    o.target = data.nodes[o.target];
  }
  // prepare data struct to also carry our 'path helper nodes':
  data.helpers = {left: {}, right: {}};

  hullg = vis.append("g");
  if (debug) {
    linkg = vis.append("g");
    helper_nodeg = vis.append("g");
  }
  helper_linkg = vis.append("g");
  nodeg = vis.append("g");
  if (debug == 1) {
    node = vis.append("g").append("circle")
        .attr("class", "center-of-mass")
        .attr("r", 10);
  }

  init();

  vis.attr("opacity", 1e-6)
    .transition()
    .duration(1000)
    .attr("opacity", 1);
});

function init() {
  /*
  We're kinda lazy with maintaining the anti-coll grid here: only when we hit a 'occupied' node,
  do we go and check if the occupier is still there, updating his quant grid location.

  This works because it 'evens out over time': a tested node hitting an 'unoccupied slot' takes that
  slot, so at the start, everybody might think they've got a free slot for themselves, then on the
  next 'tick', the slot may be suddenly found occupied by someone else also sitting in the same slot,
  causing double occupations to be resolved as the marked owner will stay, while all the others will
  be pushed out.

  As we'll have a lot of 'ticks' before the shows stops, we'll have plenty of time to get everybody
  to an actually really empty grid slot.

  Note that the feature set lists this as 'first come, first serve', but when you read this, I'm sure
  you realize that's a bit of a lie. After all, it's only really 'first come, first serve in nodes[]
  order' on the INITIAL ROUND, isn't it?
  */
  var anticollision_grid = [], xquant = 1, yquant = 1, xqthresh, yqthresh;

  if (force) force.stop();

  net = network(data, net);

  force = d3.layout.force()
      .nodes(net.nodes)
      .links(net.links)
      .size([width, height])
      .linkDistance(function(l, i) {
        //return 300;
        var n1 = l.source, n2 = l.target,
            g1 = n1.group_data || n1, g2 = n2.group_data || n2,
            n1_is_group = n1.size || 0, n2_is_group = n2.size || 0,
            rv = 300;
        // larger distance for bigger groups:
        // both between single nodes and _other_ groups (where size of own node group still counts),
        // and between two group nodes.
        //
        // reduce distance for groups with very few outer links,
        // again both in expanded and grouped form, i.e. between individual nodes of a group and
        // nodes of another group or other group node or between two group nodes.
        //
        // The latter was done to keep the single-link groups close.
        if (n1.group == n2.group) {
          if ((n1.link_count < 2 && !n1_is_group) || (n2.link_count < 2 && !n2_is_group)) {
            // 'real node' singles: these don't need a big distance to make the distance, if you whumsayin' ;-)
            rv = 2;
          } else if (!n1_is_group && !n2_is_group) {
            rv = 2;
          } else if (g1.link_count < 4 || g2.link_count < 4) {
            rv = 100;
          }
        } else {
          if (!n1_is_group && !n2_is_group) {
            rv = 50;
          } else if ((n1_is_group && n2_is_group) && (g1.link_count < 4 || g2.link_count < 4)) {
            // 'real node' singles: these don't need a big distance to make the ditance, if you whumsayin' ;-)
            rv = 100;
          } else if ((n1_is_group && g1.link_count < 2) || (n2_is_group && g2.link_count < 2)) {
            // 'real node' singles: these don't need a big distance to make the ditance, if you whumsayin' ;-)
            rv = 30;
          } else if (!n1_is_group || !n2_is_group) {
            rv = 100;
          }
        }
        return l.distance = rv;
      })
      .gravity(1.0)             // gravity+charge tweaked to ensure good 'grouped' view (e.g. green group not smack between blue&orange, ...
      .charge(function(d, i) {  // ... charge is important to turn single-linked groups to the outside
        if (d.size > 0) {
          return -5000;  // group node
        } else {
          // 'regular node'
          return -1000;
        }
      })
      .friction(0.7)   // friction adjusted to get dampened display: less bouncy bouncy ball [Swedish Chef, anyone?]
      .start();

  /*
  And here's the crazy idea for allowing AND rendering multiple links between 2 nodes, etc., as the initial attempt
  to include the 'helper' nodes in the basic 'force' failed dramatically from a visual PoV: we 'overlay' the basic
  nodes+links force with a SECOND force layout which 'augments' the original force layout by having it 'layout' all
  the helper nodes (with their links) between the 'fixed' REAL nodes, which are laid out by the original force.

  This way, we also have the freedom to apply a completely different force field setup to the helpers (no gravity
  as it doesn't make sense for helpers, different charge values, etc.).
  */
  force2 = d3.layout.force()
      .nodes(net.helper_nodes)
      .links(net.helper_links)
      .size([width, height])
      .linkDistance(function(l, i) {
        var n1 = l.real_source, n2 = l.real_target, rv,
            lr = l.g_ref,
            n1r, n2r,
            dx, dy;
        if (lr.source.size > 0 || lr.target.size > 0)
          return 20;
        return 1;
      })
      .gravity(0.0)   // just a tad of gravidy to help keep those curvy buttocks decent
      .charge(function(d, i) {
        // helper nodes have a medium-to-high charge, depending on the number of links the related force link represents.
        // Hence bundles of links fro A->B will have helper nodes with huge charges: better spreading of the link paths.
        //
        // Unless we're looking at helpers for links between 'real nodes', NOT GROUPS: in that case we want to keep
        // the lines are straight as posssible as there would only be one relation for A->B anyway, so we lower the charge
        // for such nodes and helpers.
        if (d.fixed)
          return -10;
        var l = d.link_ref,
            c = l.link_count || 1;
        if (l.source.size > 0 || l.target.size > 0)
          return -30;
        return -1;
      })
      .friction(0.95)
      .start()
      .stop();          // and immediately stop! force.tick will drive this one every tick!

  hullg.selectAll("path.hull").remove();
  hull = hullg.selectAll("path.hull")
      .data(convexHulls(net.nodes, off))
      .enter().append("path")
        .attr("class", "hull")
        .attr("d", drawCluster)
        .style("fill", function(d) { return fill(d.group); })
        .on("click", on_hull_click);

  if (debug == 1) {
    link = linkg.selectAll("line.link").data(net.links, linkid);
    link.exit().remove();
    link.enter().append("line")
        .attr("class", "link")
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
    // both existing and enter()ed links may have changed stroke width due to expand state change somewhere:
    link.style("stroke-width", function(d) { return d.size || 1; });
  }

  hlink = helper_linkg.selectAll("path.hlink").data(net.helper_render_links, function(d) {
    return d.id;
  });
  hlink.exit().remove();
  hlink.enter().append("path")
      .attr("class", "hlink");
  // both existing and enter()ed links may have changed stroke width due to expand state change somewhere:
  hlink.style("stroke-width", function(d) { return d.size || 1; });

  if (debug) {
    hnode = helper_nodeg.selectAll("circle.node").data(net.helper_nodes, function(d) {
      return d.id;
    });
    hnode.exit().remove();
    hnode.enter().append("circle")
        // if (d.size) -- d.size > 0 when d is a group node.
        // d.size < 0 when d is a 'path helper node'.
        .attr("class", function(d) {
          return "node" + (d.size > 0 ? "" : d.size < 0 ? " helper" : " leaf");
        })
        .attr("r", function(d) {
          return d.size > 0 ? d.size + dr : d.size < 0 ? 2 : dr + 1;
        })
        .attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; })
        .style("fill", function(d) { return fill(d.group); });
  }

  node = nodeg.selectAll("circle.node").data(net.nodes, nodeid);
  node.exit().remove();
  node.enter().append("circle")
      // if (d.size) -- d.size > 0 when d is a group node.
      // d.size < 0 when d is a 'path helper node'.
      .attr("class", function(d) {
        return "node" + (d.size > 0 ? d.expansion ? " link-expanded" : "" : " leaf");
      })
      .attr("r", function(d) {
        return d.size > 0 ? d.size + dr : dr + 1;
      })
      .attr("cx", function(d) { return d.x; })
      .attr("cy", function(d) { return d.y; })
      .style("fill", function(d) { return fill(d.group); })
      .on("click", on_node_click);

  node.call(force.drag);

  var drag_in_progress = false;
  var change_squared;

  // CPU load redux for the fix, part 3: jumpstart the annealing process again when the user moves the mouse outside the node,
  // when we believe the drag is still going on; even when it isn't anymore, but D3 doesn't inform us about that!
  node
    .on("mouseout.ger_fix", function(d) {
      if (debug == 1) console.log("mouseout.ger_fix", this, arguments, d.fixed, drag_in_progress);
      if (drag_in_progress) {
        force.resume();
      }
    });

  var resume_threshold = 0.05;

  force.on("tick", function(e) {
    /*
    Force all nodes with only one link to point outwards.

    To do this, we first calculate the center mass (okay, we wing it, we fake node 'weight'),
    then see whether the target node for links from single-link nodes is closer to the
    center-of-mass than us, and if it isn't, we push the node outwards.
    */
    var center = {x: 0, y: 0, weight: 0}, singles = [],
        size, c, k, mx, my, dx, dy, alpha;

    drag_in_progress = false;
    net.nodes.forEach(function(n) {
      var w = Math.max(1, n.size || 0, n.weight || 0);

      center.x += w * n.x;
      center.y += w * n.y;
      center.weight += w;

      if (n.fixed & 2) {
        drag_in_progress = true;
      }

      if (n.size > 0 ? n.link_count < 4 : n.group_data.link_count < 3)
        singles.push(n);
    });

    size = force.size();

    mx = size[0] / 2;
    my = size[1] / 2;

    singles.forEach(function(n) {
      var l = n.first_link, n2 = n.first_link_target,
          proj, ax, bx, ay, by, k, x, y, alpha, rej, power,
          dx, dy,
          n_is_group = n.size || 0,
          ng = n.group_data || n,
          c2,
          w = Math.max(1, n.size || 0, n.weight || 0);

      // haven't decided what to do for unconnected nodes, yet...
      if (!l)
        return;

      // apply amplification of the 'original' alpha:
      // 1.0 for singles and double-connected nodes, close to 0 for highly connected nodes, rapidly decreasing.
      // Use this as we want to give those 'non-singles' a little bit of the same 'push out' treatment.
      // Reduce effect for 'real nodes' which are singles: they need much less encouragement!
      power = Math.max(2, n_is_group ? n.link_count : n.group_data.link_count);
      power = 2 / power;

      alpha = e.alpha * power;

      // undo/revert gravity forces (or as near as we can get, here)
      //
      // revert for truely single nodes, revert just a wee little bit for dual linked nodes,
      // only reduce ever so slighty for nodes with few links (~ 3) that made it into this
      // 'singles' selection
      if (k = alpha * force.gravity() * (0.8 + power)) {
        dx = (mx - n.x) * k;
        dy = (my - n.y) * k;
        n.x -= dx;
        n.y -= dy;

        center.x -= dx * w;
        center.y -= dy * w;
      }
    });

    // move the entire graph so that its center of mass sits at the center, period.
    center.x /= center.weight;
    center.y /= center.weight;

    if (debug == 1) {
      c = vis.selectAll("circle.center-of-mass")
          .attr("cx", center.x)
          .attr("cy", center.y);
    }

    dx = mx - center.x;
    dy = my - center.y;

    alpha = e.alpha * 5;
    dx *= alpha;
    dy *= alpha;

    net.nodes.forEach(function(n) {
      n.x += dx;
      n.y += dy;
    });


    change_squared = 0;

    // fixup .px/.py so drag behaviour and annealing get the correct values, as
    // force.tick() would expect .px and .py to be the .x and .y of yesterday.
    net.nodes.forEach(function(n) {
      // restrain all nodes to window area
      var k, dx, dy,
          r = (n.size > 0 ? n.size + dr : dr + 1) + 2 /* styled border outer thickness and a bit */;

      dx = 0;
      if (n.x < r)
        dx = r - n.x;
      else if (n.x > size[0] - r)
        dx = size[0] - r - n.x;

      dy = 0;
      if (n.y < r)
        dy = r - n.y;
      else if (n.y > size[1] - r)
        dy = size[1] - r - n.y;

      k = 1.2;

      n.x += dx * k;
      n.y += dy * k;
      // restraining completed.......................

      // fixes 'elusive' node behaviour when hovering with the mouse (related to force.drag)
      if (n.fixed) {
        // 'elusive behaviour' ~ move mouse near node and node would take off, i.e. act as an elusive creature.
        n.x = n.px;
        n.y = n.py;
      }
      n.px = n.x;
      n.py = n.y;

      // plus copy for faster stop check
      change_squared += (n.qx - n.x) * (n.qx - n.x);
      change_squared += (n.qy - n.y) * (n.qy - n.y);
      n.qx = n.x;
      n.qy = n.y;
    });

    // kick the force2 to also do a bit of annealing alongside:
    // to make it do something, we need to surround it alpha-tweaking stuff, though.
    force2.resume();
    force2.tick();
    force2.stop();

    // fast stop + the drag fix, part 2:
    if (change_squared < .005) {
      if (debug == 1) console.log("fast stop: CPU load redux");
      force.stop();
      // fix part 4: monitor D3 resetting the drag marker:
      if (drag_in_progress) {
        if (debug == 1) console.log("START monitor drag in progress", drag_in_progress);
        d3.timer(function() {
          drag_in_progress = false;
          net.nodes.forEach(function(n) {
            if (n.fixed & 2) {
              drag_in_progress = true;
            }
          });
          force.resume();
          if (debug == 1) console.log("monitor drag in progress: drag ENDED", drag_in_progress);
          // Quit monitoring as soon as we noticed the drag ENDED.
          // Note: we continue to monitor at +500ms intervals beyond the last tick
          //       as this timer function ALWAYS kickstarts the force layout again
          //       through force.resume().
          //       d3.timer() API only accepts an initial delay; we can't set this
          //       thing to scan, say, every 500msecs until the drag is done,
          //       so we do it that way, via the revived force.tick process.
          return true;
        }, 500);
      }
    } else if (change_squared > net.nodes.length * 5 && e.alpha < resume_threshold) {
      // jolt the alpha (and the visual) when there's still a lot of change when we hit the alpha threshold.
      force.alpha(Math.min(0.1, e.alpha *= 2)); //force.resume(), but now with decreasing alpha starting value so the jolts don't get so big.

      // And 'dampen out' the trigger point, so it becomes harder and harder to trigger the threshold.
      // This is done to cope with those instable (forever rotating, etc.) layouts...
      resume_threshold *= 0.9;
    }

    //--------------------------------------------------------------------

    if (!hull.empty()) {
      hull.data(convexHulls(net.nodes, off))
          .attr("d", drawCluster);
    }

    if (debug == 1) {
      link.attr("x1", function(d) { return d.source.x; })
          .attr("y1", function(d) { return d.source.y; })
          .attr("x2", function(d) { return d.target.x; })
          .attr("y2", function(d) { return d.target.y; });
    }

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });
  });

  force2.on("tick", function(e) {
    /*
      Update all 'real'=fixed nodes.
    */
    net.helper_nodes.forEach(function(n) {
      var o;
      if (n.fixed) {
        o = n.ref;
        n.px = n.x = o.x;
        n.py = n.y = o.y;
      }
    });
    net.helper_links.forEach(function(l) {
      var o = l.g_ref;
      l.distance = o.distance;
    });

    // NOTE: force2 is fully driven by force(1), but still there's need for 'fast stop' handling in here
    //       as our force2 may be more 'joyous' in animating the links that force is animating the nodes
    //       themselves. Hence we also take the delta movement of the helper nodes into account!
    net.helper_nodes.forEach(function(n) {
      // skip the 'fixed' buggers: those are already accounted for in force.tick!
      if (n.fixed)
        return;

      // plus copy for faster stop check
      change_squared += (n.qx - n.x) * (n.qx - n.x);
      change_squared += (n.qy - n.y) * (n.qy - n.y);
      n.qx = n.x;
      n.qy = n.y;
    });

    //--------------------------------------------------------------------

    hlink.attr("d", function(d) {
      var linedata = [
          [d.real_source.x, d.real_source.y],
          [d.source.x, d.source.y],
          [d.target.x, d.target.y],
          [d.real_target.x, d.real_target.y]
      ];
      return pathgen(linedata);
    });

    if (debug) {
      hnode.attr("cx", function(d) { return d.x; })
           .attr("cy", function(d) { return d.y; });
    }
  });
}