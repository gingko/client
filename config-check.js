const configReference = require("./config-example.js");
const config = require("./config.js");
const _ = require("lodash");
const process = require("process");

let confRefKeys = Object.keys(configReference).sort();
let confKeys = Object.keys(config).sort();

let diff = _.xor(confRefKeys, confKeys);

if (diff.length != 0) {
  process.stderr.write("Difference in config keys: " + JSON.stringify(diff));
}
