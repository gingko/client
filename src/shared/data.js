Object.defineProperty(Array.prototype, "tap", { value(f) { f(this); return this; }});


function startPullingChanges (localDb, remoteDb, treeId) {
  let options = {selector : { _id: { $regex: `${treeId}/` } }, live : true, retry : true};
  localDb.replicate.from(remoteDb, options).on('change', async (change) => {
    if (includesRef(change)) {
      let allDocsRes = await allTreeDocs(localDb, treeId);
      let allDocs = allDocsResToDocs(allDocsRes);
      let docsWithConflicts = await getConflicts(localDb, allDocs);
      console.log("docsWithConflicts", docsWithConflicts);
    }
  });
}


async function saveData(localDb, treeId, elmData, savedImmutablesIds) {
  // Filter out already saved immutable objects.
  // Add treeId prefix.
  let toSave =
    elmData
      .tap(console.log)
      .filter(d => !savedImmutablesIds.includes(treeId + "/" + d._id))
      .tap(console.log)
      .map(d => prefix(d, treeId))

  // Save local head as _local PouchDB document.
  // This allows us to later figure out which conflicting revision
  // was picked as the arbitrary winner (the local or remote one).
  let localHeadToSave =
    toSave
      .filter(d => d._id === `${treeId}/heads/master`)
      .map(lh => { lh._id = `_local/${treeId}/heads/master`; return lh; })
      [0];
  await saveLocalHead(localDb, localHeadToSave);

  // Return responses of successfully saved documents.
  let saveResponses = await localDb.bulkDocs(toSave);
  return saveResponses.filter(r => r.ok).map(r => {delete r.ok; return r;});
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



function allTreeDocs (db, treeId) {
  let options = {include_docs: true , conflicts : true, startkey: treeId + "/", endkey: treeId + "/\ufff0"};
  return db.allDocs(options);
}


function allDocsResToDocs (allDocsRes) {
  return allDocsRes.rows.map(r => r.doc);
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


function unprefix(doc, treeId) {
  let newDoc = Object.assign({},doc);
  let newId = id.slice(treeId.length + 1);
  newDoc._id = newId;
  return newDoc;
}




/* === Exports === */

export { saveData, startPullingChanges };