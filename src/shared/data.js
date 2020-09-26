import _ from "lodash";

Object.defineProperty(Array.prototype, "tap", { value(f) { f(this); return this; }});


async function load (localDb, treeId) {
  let allDocs = await loadAll(localDb, treeId);
  let elmDocs = loadedResToElmData(allDocs, treeId);
  let conflictedDocs = await getAllConflicts(localDb, allDocs, treeId);
  let localHead = await loadLocalHead(localDb, treeId).catch(e => null);
  return maybeAddConflict(elmDocs, conflictedDocs, localHead);
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

  // Save documents and return responses of successfully saved ones.
  let saveResponses = await localDb.bulkDocs(toSave);
  let successfulResponses =
    saveResponses
      .filter(r => r.ok)
      .map(r => {delete r.ok; return r;})
      .map(d => unprefix(d, treeId, "id"))

  // Check if we've resolved a merge conflict
  let conflictsExist;
  let [head, headConflicts] = await getHeadAndConflicts(localDb, treeId);
  if (headConflicts.length > 0) {
    // If winning rev is greater than conflicting ones
    // then the conflict was resolved. Delete conflicting revs.
    if (revToInt(head._rev) > revToInt(headConflicts[0]._rev)) {
      let confDelPromises = headConflicts.map(confDoc => localDb.remove(confDoc._id, confDoc._rev));
      await Promise.allSettled(confDelPromises);
      conflictsExist = false;
    } else {
      conflictsExist = true
    }
  } else {
    conflictsExist = false;
  }

  // Get ids of successfully saved immutable objects.
  let immutableObjFilter = (d) => !d.id.includes("heads/master") && !d.id.includes("metadata");
  let newSavedImmutables = successfulResponses.filter(immutableObjFilter).map(r => r.id);


  return [successfulResponses, newSavedImmutables, conflictsExist];
}


async function push(localDb, remoteDb, treeId, checkForConflicts) {
  let shouldPush = true;
  if (checkForConflicts) {
    let allDocs = await loadAll(localDb, treeId);
    let conflictedDocs = await getAllConflicts(localDb, allDocs, treeId);
    shouldPush = conflictedDocs.length === 0;
  }

  if (shouldPush) {
    let selector = { _id: { $regex: `${treeId}/` } };
    localDb.replicate.to(remoteDb, { selector }).catch(async (e) => e);
  }
}


async function loadAll(localDb, treeId) {
  let options = {include_docs: true , conflicts : true, startkey: treeId + "/", endkey: treeId + "/\ufff0"};
  let allDocsRes = await localDb.allDocs(options);
  return allDocsRes.rows.map(r => r.doc);
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


function loadLocalHead(localDb, treeId) {
  return localDb.get(`_local/${treeId}/heads/master`)
}


async function getHeadAndConflicts(localDb, treeId) {
  let headId = `${treeId}/heads/master`;
  let head = await localDb.get(headId, {conflicts: true});

  let conflictPromises = [];
  if (head.hasOwnProperty("_conflicts")) {
    conflictPromises =
      head._conflicts.map(confRev => localDb.get(headId, {rev: confRev}));
  }
  let resolved = await Promise.allSettled(conflictPromises);

  let retVal =
    resolved
      .filter(p => p.status === "fulfilled")
      .map(p => p.value)

  return [head, retVal];
}


async function getAllConflicts (db, allDocs, treeId) {
  let idConfTupleMap = (id, confRevs) => {
    return confRevs.flatMap(confRev => db.get(id, {rev: confRev}));
  }
  let getConflictPromises =
    allDocs
      .filter(d => d.hasOwnProperty("_conflicts"))
      .map(d => [d._id, d._conflicts])
      .flatMap(tup => idConfTupleMap(tup[0],tup[1]))

  let resolved = await Promise.allSettled(getConflictPromises);

  return resolved
    .filter(p => p.status === "fulfilled")
    .map(p => p.value)
    .map(d => unprefix(d, treeId));
}


function maybeAddConflict(elmDocs, conflictingDocs, savedLocalHead) {
  if (conflictingDocs.length === 0) { return elmDocs }

  let newDocs = Object.assign({}, elmDocs);
  let headId = `heads/master`;

  let losingHead = conflictingDocs.filter(d => d._id === headId)[0];
  let winningHead = newDocs.ref.filter(d => d._id === headId)[0];

  if (losingHead.value === savedLocalHead.value) {
    // Flip
    newDocs.ref = newDocs.ref.map(d => d._id === headId ? losingHead : d);
    newDocs.conflict = winningHead;
  } else {
    newDocs.conflict = losingHead;
  }
  return newDocs;
}


function loadedResToElmData (docs, treeId) {
  let newDocs = docs.map(d => unprefix(d, treeId));
  let groupFn = (r) => (r.hasOwnProperty("type") ? r.type : r._id);
  return _.groupBy(newDocs, groupFn);
}




/* === Helper Functions === */

function revToInt (rev) {
  return Number(rev.split('-')[0]);
}


function includesRef (change) {
  return change.docs.map(d => d.type).includes("ref");
}


function prefix(doc, treeId) {
  let newDoc = Object.assign({},doc);
  newDoc._id = treeId + "/" + doc._id;
  return newDoc;
}


function unprefix(doc, treeId, idField = "_id") {
  let newDoc = Object.assign({},doc);
  newDoc[idField] = doc[idField].slice(treeId.length + 1);
  return newDoc;
}




/* === Exports === */

export { load, startPullingChanges, saveData, push };