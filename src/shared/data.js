import _ from "lodash";

Object.defineProperty(Array.prototype, "tap", { value(f) { f(this); return this; }});


async function load (localDb, treeId) {
  let options = {include_docs: true , conflicts : true, startkey: treeId + "/", endkey: treeId + "/\ufff0"};
  let allDocsRes = await localDb.allDocs(options);
  let allDocs = allDocsRes.rows.map(r => r.doc).map(d => unprefix(d, treeId));
  let toSend = loadedResToElmData(allDocs, treeId);
  let docsWithConflicts = await getConflicts(localDb, allDocs);
  return toSend;
}


function startPullingChanges (localDb, remoteDb, treeId, changeHandler) {
  let options = {selector : { _id: { $regex: `${treeId}/` } }, live : true, retry : true};
  localDb.replicate.from(remoteDb, options).on('change', async (change) => {
    if (includesRef(change)) {
      let toSend = await load(localDb, treeId);
      changeHandler(toSend);
    }
  });
}


async function saveData(localDb, treeId, elmData, savedImmutablesIds) {
  // Filter out already saved immutable objects.
  // Add treeId prefix.
  let toSave =
    elmData
      .filter(d => !savedImmutablesIds.includes(treeId + "/" + d._id))
      .map(d => prefix(d, treeId))

  // Save local head as _local PouchDB document.
  // This allows us to later figure out which conflicting revision
  // was picked as the arbitrary winner (the local or remote one).
  let makeIdLocal = (doc) => {
    let newDoc = Object.assign({}, doc);
    newDoc._id = `_local/${treeId}/heads/master`;
    return newDoc;
  }
  let localHeadToSave =
    toSave
      .filter(d => d._id === `${treeId}/heads/master`)
      .map(makeIdLocal)
      [0];
  await saveLocalHead(localDb, localHeadToSave);

  // Return responses of successfully saved documents.
  // Also return ids of successfully saved immutable objects.
  let saveResponses = await localDb.bulkDocs(toSave);
  let successfulResponses =
    saveResponses
      .filter(r => r.ok)
      .map(r => {delete r.ok; return r;})
      .map(d => unprefix(d, treeId, "id"))

  let immutableObjFilter = (d) => !d.id.includes("heads/master") && !d.id.includes("metadata");
  let newSavedImmutables = successfulResponses.filter(immutableObjFilter).map(r => r.id);
  return [successfulResponses, newSavedImmutables];
}


function push(localDb, remoteDb, treeId) {
  let selector = { _id: { $regex: `${treeId}/` } };
  localDb.replicate.to(remoteDb, { selector }).catch(async (e) => e);
}


async function saveLocalHead(localDb, headToSave) {
  try {
    let oldLocalHead = await localDb.get(headToSave._id);
    headToSave._rev = oldLocalHead._rev
  } catch (e) {
    delete headToSave._rev;
  }
  return localDb.put(headToSave);
}


async function getConflicts (db, allDocs) {
  let idConfTupleMap = (id, confRevs) => {
    return confRevs.flatMap(confRev => db.get(id, {rev: confRev}));
  }
  let getConflictPromises =
    allDocs
      .filter(d => d.hasOwnProperty("_conflicts"))
      .map(d => [d._id, d._conflicts])
      .flatMap(tup => idConfTupleMap(tup[0],tup[1]))

  let resolved = await Promise.allSettled(getConflictPromises);
  return resolved.filter(p => p.status === "fulfilled").map(p => p.value);
}


function loadedResToElmData (docs) {
  let groupFn = (r) => (r.hasOwnProperty("type") ? r.type : r._id);
  let sth = _.groupBy(docs, groupFn);
  return sth;
}




/* === Helper Functions === */

function includesRef (change) {
  return change.docs.map(d => d.type).includes("ref");
}


function prefix(doc, treeId) {
  let newDoc = Object.assign({},doc);
  let newId = treeId + "/" + doc._id;
  newDoc._id = newId;
  return newDoc;
}


function unprefix(doc, treeId, idField = "_id") {
  let newDoc = Object.assign({},doc);
  let newId = doc[idField].slice(treeId.length + 1);
  newDoc[idField] = newId;
  return newDoc;
}




/* === Exports === */

export { load, startPullingChanges, saveData, push };