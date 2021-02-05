import * as data from "./data.js";

onmessage = async function(e) {
  console.log('Message received from main script', e);
  switch (e.data.tag) {
    case "newSave":
      let d = e.data.data;
      let [ savedData
        , savedImmutables
        , conflictsExist
        , savedMetadata
      ] = await data.newSave(d.db, d.treeId, d.elmData, d.timestamp, d.savedImmutables);
      break;
  }
}