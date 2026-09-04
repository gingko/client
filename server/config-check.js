import configReference from "./config-example.js";
import config from "./config.js";
import process from "process";

let confRefKeys = Object.keys(configReference).sort();
let confKeys = Object.keys(config).sort();

let diff = confRefKeys
            .filter(x => !confKeys.includes(x))
            .concat(confKeys.filter(x => !confRefKeys.includes(x)));

if (diff.length != 0) {
  process.stderr.write("Difference in config keys: " + JSON.stringify(diff));
}
