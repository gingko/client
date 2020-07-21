const _ = require("lodash");
const React = require("react");
const ReactDOM = require("react-dom");
const CommitsGraph = require("react-commits-graph");


class GitGraph extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.mountPoint = document.createElement("div");
    this.attachShadow({ mode: "open" }).appendChild(this.mountPoint);
    this.mountPoint.style = "width: auto; height: auto;";
    this.render();
  }

  static get observedAttributes() {
    return ["commits"];
  }

  attributeChangedCallback(name, oldVal, newVal) {
    if(oldVal !== newVal) {
      this.render();
    }
  }

  render() {
    var commitElement = React.createElement(CommitsGraph,
      {
        commits: this.commits,
        selected: this.selected || null,
        width: 80,
        height: 1000,
        y_step: 20,
        x_step: 20,
        dotRadius: 3,
        lineWidth: 2,
        mirror: false,
        unstyled: false
      });

    ReactDOM.render(commitElement, this.mountPoint);
  }

}

customElements.define("git-graph", GitGraph);

