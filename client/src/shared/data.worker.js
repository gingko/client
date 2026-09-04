import * as data from "./data.js";

onmessage = async function(e) {
  switch (e.data.tag) {
    case "newSave":
      let d = e.data.data;
      let result = await data.newSave(d.db, d.treeId, d.elmData, d.timestamp, d.savedImmutables);
      postMessage({tag: "CommitDataResult", data: result});
      break;
  }
}