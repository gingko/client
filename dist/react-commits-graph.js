/*
Generate preformatted data of commits graph.
 */
var Node, generateGraphData, remove;

generateGraphData = function(commits) {

  /*
  Generate graph data.
  
  :param commits: a list of commit, which should have
    `sha`, `parents` properties.
  :returns: data nodes, a json list of
    [
      sha,
      [offset, branch], //dot
      [
      [from, to, branch],  // route 1
      [from, to, branch],  // route 2
      [from, to, branch],
      ]  // routes
    ],  // node
   */
  var b, branch, branchIndex, branches, commit, getBranch, i, j, k, l, len, len1, len2, len3, len4, m, n, node, nodes, numParents, offset, otherBranch, ref, ref1, reserve, routes;
  nodes = [];
  branchIndex = [0];
  reserve = [];
  branches = {};
  getBranch = function(sha) {
    if (branches[sha] == null) {
      branches[sha] = branchIndex[0];
      reserve.push(branchIndex[0]);
      branchIndex[0]++;
    }
    return branches[sha];
  };
  for (j = 0, len = commits.length; j < len; j++) {
    commit = commits[j];
    branch = getBranch(commit.sha);
    numParents = commit.parents.length;
    offset = reserve.indexOf(branch);
    routes = [];
    if (numParents === 1) {
      if (branches[commit.parents[0]] != null) {
        ref = reserve.slice(offset + 1);
        for (i = k = 0, len1 = ref.length; k < len1; i = ++k) {
          b = ref[i];
          routes.push([i + offset + 1, i + offset + 1 - 1, b]);
        }
        ref1 = reserve.slice(0, offset);
        for (i = l = 0, len2 = ref1.length; l < len2; i = ++l) {
          b = ref1[i];
          routes.push([i, i, b]);
        }
        remove(reserve, branch);
        routes.push([offset, reserve.indexOf(branches[commit.parents[0]]), branch]);
      } else {
        for (i = m = 0, len3 = reserve.length; m < len3; i = ++m) {
          b = reserve[i];
          routes.push([i, i, b]);
        }
        branches[commit.parents[0]] = branch;
      }
    } else if (numParents === 2) {
      branches[commit.parents[0]] = branch;
      for (i = n = 0, len4 = reserve.length; n < len4; i = ++n) {
        b = reserve[i];
        routes.push([i, i, b]);
      }
      otherBranch = getBranch(commit.parents[1]);
      routes.push([offset, reserve.indexOf(otherBranch), otherBranch]);
    }
    node = Node(commit.sha, offset, branch, routes);
    nodes.push(node);
  }
  return nodes;
};

remove = function(list, item) {
  list.splice(list.indexOf(item), 1);
  return list;
};

Node = function(sha, offset, branch, routes) {
  return [sha, [offset, branch], routes];
};

SVGPathData = (function() {
  function SVGPathData() {
    this.commands = [];
  }

  SVGPathData.prototype.toString = function() {
    return this.commands.join(' ');
  };

  SVGPathData.prototype.moveTo = function(x, y) {
    return this.commands.push("M " + x + "," + y);
  };

  SVGPathData.prototype.lineTo = function(x, y) {
    return this.commands.push("L " + x + "," + y);
  };

  SVGPathData.prototype.closePath = function(x, y) {
    return this.commands.push("Z");
  };

  SVGPathData.prototype.bezierCurveTo = function(cp1x, cp1y, cp2x, cp2y, x, y) {
    return this.commands.push("C " + cp1x + ", " + cp1y + ", " + cp2x + ", " + cp2y + ", " + x + ", " + y);
  };

  SVGPathData.prototype.quadraticCurveTo = function(cp1x, cp1y, x, y) {
    return this.commands.push("Q " + cp1x + ", " + cp1y + ", " + x + ", " + y);
  };

  return SVGPathData;

})();

var COLOURS, CommitsGraphMixin, React, SVGPathData, branchCount, classSet, distance, generateGraphData, getColour,
  slice = [].slice;

COLOURS = ["#e11d21", "#fbca04", "#009800", "#006b75", "#207de5", "#0052cc", "#5319e7", "#f7c6c7", "#fad8c7", "#fef2c0", "#bfe5bf", "#c7def8", "#bfdadc", "#bfd4f2", "#d4c5f9", "#cccccc", "#84b6eb", "#e6e6e6", "#ffffff", "#cc317c"];

classSet = function() {
  var classes;
  classes = 1 <= arguments.length ? slice.call(arguments, 0) : [];
  return classes.filter(Boolean).join(' ');
};

getColour = function(branch) {
  var n;
  n = COLOURS.length;
  return COLOURS[branch % n];
};

branchCount = function(data) {
  var i, j, maxBranch;
  maxBranch = -1;
  i = 0;
  while (i < data.length) {
    j = 0;
    while (j < data[i][2].length) {
      if (maxBranch < data[i][2][j][0] || maxBranch < data[i][2][j][1]) {
        maxBranch = Math.max.apply(Math, [data[i][2][j][0], data[i][2][j][1]]);
      }
      j++;
    }
    i++;
  }
  return maxBranch + 1;
};

distance = function(point1, point2) {
  var xs, ys;
  xs = 0;
  ys = 0;
  xs = point2.x - point1.x;
  xs = xs * xs;
  ys = point2.y - point1.y;
  ys = ys * ys;
  return Math.sqrt(xs + ys);
};

CommitsGraphMixin = {
  getDefaultProps: function() {
    return {
      y_step: 20,
      x_step: 20,
      dotRadius: 3,
      lineWidth: 2,
      selected: null,
      mirror: false,
      unstyled: false
    };
  },
  componentWillReceiveProps: function() {
    this.graphData = null;
    return this.branchCount = null;
  },
  cursorPoint: function(e) {
    var svg, svgPoint;
    svg = ReactDOM.findDOMNode(this);
    svgPoint = svg.createSVGPoint();
    svgPoint.x = e.clientX;
    svgPoint.y = e.clientY;
    return svgPoint.matrixTransform(svg.getScreenCTM().inverse());
  },
  handleClick: function(e) {
    var base, closestCommit, commit, commitDistance, cursorLoc, k, len, ref, smallestDistance;
    cursorLoc = this.cursorPoint(e);
    smallestDistance = 2e308;
    closestCommit = null;
    ref = this.renderedCommitsPositions;
    for (k = 0, len = ref.length; k < len; k++) {
      commit = ref[k];
      commitDistance = distance(cursorLoc, commit);
      if (commitDistance < smallestDistance) {
        smallestDistance = commitDistance;
        closestCommit = commit;
      }
    }
    return typeof (base = this.props).onClick === "function" ? base.onClick(closestCommit.sha) : void 0;
  },
  getGraphData: function() {
    return this.graphData || (this.graphData = generateGraphData(this.props.commits));
  },
  getBranchCount: function() {
    return this.branchCount || (this.branchCount = branchCount(this.getGraphData()));
  },
  getWidth: function() {
    if (this.props.width != null) {
      return this.props.width;
    }
    return this.getContentWidth();
  },
  getContentWidth: function() {
    return (this.getBranchCount() + 0.5) * this.props.x_step;
  },
  getHeight: function() {
    if (this.props.height != null) {
      return this.props.height;
    }
    return this.getContentHeight();
  },
  getContentHeight: function() {
    return (this.getGraphData().length + 2) * this.props.y_step;
  },
  getInvert: function() {
    if (this.props.mirror) {
      return 0 - this.props.width;
    } else {
      return 0;
    }
  },
  getOffset: function() {
    return this.getWidth() / 2 - this.getContentWidth() / 2;
  },
  renderRouteNode: function(svgPathDataAttribute, branch) {
    var classes, colour, style;
    if (!this.props.unstyled) {
      colour = getColour(branch);
      style = {
        'stroke': colour,
        'stroke-width': this.props.lineWidth,
        'fill': 'none'
      };
    }
    classes = "commits-graph-branch-" + branch;
    return React.DOM.path({
      d: svgPathDataAttribute,
      style: style,
      className: classes
    });
  },
  renderRoute: function(commit_idx, arg) {
    var branch, from, from_x, from_y, invert, offset, ref, svgPath, to, to_x, to_y, x_step, y_step;
    from = arg[0], to = arg[1], branch = arg[2];
    ref = this.props, x_step = ref.x_step, y_step = ref.y_step;
    offset = this.getOffset();
    invert = this.getInvert();
    svgPath = new SVGPathData;
    from_x = offset + invert + (from + 1) * x_step;
    from_y = (commit_idx + 0.5) * y_step;
    to_x = offset + invert + (to + 1) * x_step;
    to_y = (commit_idx + 0.5 + 1) * y_step;
    svgPath.moveTo(from_x, from_y);
    if (from_x === to_x) {
      svgPath.lineTo(to_x, to_y);
    } else {
      svgPath.bezierCurveTo(from_x - x_step / 4, from_y + y_step / 3 * 2, to_x + x_step / 4, to_y - y_step / 3 * 2, to_x, to_y);
    }
    return this.renderRouteNode(svgPath.toString(), branch);
  },
  renderCommitNode: function(x, y, sha, dot_branch) {
    var classes, colour, radius, selectedClass, strokeColour, strokeWidth, style;
    radius = this.props.dotRadius;
    if (!this.props.unstyled) {
      colour = getColour(dot_branch);
      if (sha === this.props.selected) {
        strokeColour = '#000';
        strokeWidth = 2;
      } else {
        strokeColour = colour;
        strokeWidth = 1;
      }
      style = {
        'stroke': strokeColour,
        'stroke-width': strokeWidth,
        'fill': colour
      };
    }
    if (this.props.selected) {
      selectedClass = 'selected';
    }
    classes = classSet("commits-graph-branch-" + dot_branch, selectedClass);
    return React.DOM.circle({
      cx: x,
      cy: y,
      r: radius,
      style: style,
      onClick: this.handleClick,
      'data-sha': sha,
      className: classes
    });
  },
  renderCommit: function(idx, arg) {
    var commitNode, dot, dot_branch, dot_offset, index, invert, offset, ref, route, routeNodes, routes_data, sha, x, x_step, y, y_step;
    sha = arg[0], dot = arg[1], routes_data = arg[2];
    dot_offset = dot[0], dot_branch = dot[1];
    ref = this.props, x_step = ref.x_step, y_step = ref.y_step;
    offset = this.getOffset();
    invert = this.getInvert();
    x = offset + invert + (dot_offset + 1) * x_step;
    y = (idx + 0.5) * y_step;
    commitNode = this.renderCommitNode(x, y, sha, dot_branch);
    routeNodes = (function() {
      var k, len, results;
      results = [];
      for (index = k = 0, len = routes_data.length; k < len; index = ++k) {
        route = routes_data[index];
        results.push(this.renderRoute(idx, route));
      }
      return results;
    }).call(this);
    this.renderedCommitsPositions.push({
      x: x,
      y: y,
      sha: sha
    });
    return [commitNode, routeNodes];
  },
  renderGraph: function() {
    var allCommitNodes, allRouteNodes, children, commit, commitNode, height, index, k, len, ref, ref1, routeNodes, style, svgProps, width;
    this.renderedCommitsPositions = [];
    allCommitNodes = [];
    allRouteNodes = [];
    ref = this.getGraphData();
    for (index = k = 0, len = ref.length; k < len; index = ++k) {
      commit = ref[index];
      ref1 = this.renderCommit(index, commit), commitNode = ref1[0], routeNodes = ref1[1];
      allCommitNodes.push(commitNode);
      allRouteNodes = allRouteNodes.concat(routeNodes);
    }
    children = [].concat(allRouteNodes, allCommitNodes);
    height = this.getHeight();
    width = this.getWidth();
    if (!this.props.unstyled) {
      style = {
        height: height,
        width: width,
        cursor: 'pointer'
      };
    }
    svgProps = {
      height: height,
      width: width,
      style: style,
      children: children
    };
    return React.DOM.svg({
      onClick: this.handleClick,
      height: height,
      width: width,
      style: style,
      children: children
    });
  }
};

CommitsGraph = React.createClass({
  displayName: 'CommitsGraph',
  mixins: [CommitsGraphMixin],
  render: function() {
    return this.renderGraph();
  }
});
