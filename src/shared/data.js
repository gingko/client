import _ from "lodash";
import { sha1 } from "hash-wasm";

Object.defineProperty(Array.prototype, "tap", { value(f) { f(this); return this; }});

// Make initial call to load/initialize WASM code
sha1('');

async function getDocumentList(db) {
  return (await db.query("testDocList/docList").catch(async (e) => e));
}

async function load (localDb, treeId) {
  let allDocs = await loadAll(localDb, treeId);
  let elmDocs = loadedResToElmData(allDocs, treeId);
  console.log({allDocs, elmDocs});
  let conflictedDocs = await getConflicts(localDb, allDocs, treeId);
  let localHead = await loadLocalHead(localDb, treeId).catch(e => undefined);
  let immutablesIds = allDocs.map(d => d._id).filter(id => !id.includes("metadata") && !id.includes("heads"));
  return [maybeAddConflict(elmDocs, conflictedDocs, localHead), immutablesIds];
}


async function loadMetadata(localDb, treeId) {
  return localDb.get(treeId + "/metadata").catch(e => e);
}


async function saveData(localDb, treeId, elmData, savedImmutablesIds) {
  // Function to modify metadata & get its _rev.
  let updateMetadata;
  let savedMetadata;
  if (elmData.filter(d => d._id === "metadata").length > 0) {
    let oldMetadata = await localDb.get(treeId + "/metadata").catch(e => e);
    updateMetadata = d => {
      if (d._id === "metadata") {
        d.updatedAt = Date.now();
        if (oldMetadata.hasOwnProperty("_rev")) {
          d._rev = oldMetadata._rev;
          savedMetadata = Object.assign({}, d);
        }
      }
      return d;
    };
  } else {
    updateMetadata = d => d
  }

  // Filter out already saved immutable objects.
  // Add treeId prefix.
  let toSave =
    elmData
      .filter(d => !savedImmutablesIds.includes(treeId + "/" + d._id))
      .map(updateMetadata)
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
  if (typeof localHeadToSave !== "undefined") {
    await saveLocalHead(localDb, localHeadToSave);
  }

  // Save documents and return responses of successfully saved ones.
  let saveResponses = await localDb.bulkDocs(toSave);
  let successfulResponses =
    saveResponses
      .filter(r => r.ok)
      .map(r => {delete r.ok; return r;})
      .map(d => unprefix(d, treeId, "id"))


  // Get saved metadata (with new _rev), if it was saved
  let savedMetadataResponse = successfulResponses.find(r => r.id === "metadata");
  if (typeof savedMetadataResponse !== "undefined" && savedMetadataResponse.hasOwnProperty("rev")) {
    savedMetadata._rev = savedMetadataResponse.rev;
    savedMetadata = unprefix(savedMetadata, treeId);
  }

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


  return [successfulResponses, newSavedImmutables, conflictsExist, savedMetadata];
}


async function newSave(localDb, treeId, elmData, savedImmutablesIds) {
  console.time('commitTree');
  let timestamp = Date.now();
  let [commitSha, objects] = await commitTree(elmData.author, elmData.parents, elmData.workingTree, timestamp, elmData.metadata);
  console.timeEnd('commitTree');


  // Function to modify head ref & get its _rev
  let newHead = await localDb.get(treeId + "/heads/master").catch(e => e);
  newHead.value = commitSha;


  // Function to modify metadata & get its _rev.
  let savedMetadata;
  let oldMetadata = await localDb.get(treeId + "/metadata").catch(e => e);
  let updateMetadata = d => {
      if (d._id === "metadata") {
        d.updatedAt = timestamp;
        if (oldMetadata.hasOwnProperty("_rev")) {
          d._rev = oldMetadata._rev;
          savedMetadata = Object.assign({}, d);
        }
      }
      return d;
    };


  // Filter out already saved immutable objects.
  // Add treeId prefix.
  let toSave =
    objects
      .filter(d => !savedImmutablesIds.includes(treeId + "/" + d._id))
      .map(updateMetadata)
      .map(d => prefix(d, treeId))
      .concat([newHead]);

  console.log({toSave});


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
  if (typeof localHeadToSave !== "undefined") {
    await saveLocalHead(localDb, localHeadToSave);
  }


  // Save documents and return responses of successfully saved ones.
  let saveResponses = await localDb.bulkDocs(toSave);
  let getRev = (d) => {
    if (d.type === "ref") {
      let res = saveResponses.find(r => r.id == d._id);
      return Object.assign(d, {_rev: res.rev});
    } else {
      return d;
    }
  }
  let savedData =
      toSave
        .filter(d=> saveResponses.filter(r => r.ok).map(r => r.id).includes(d._id))
        .map(getRev)

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
  //let newSavedImmutables = successfulResponses.filter(immutableObjFilter).map(r => r.id);

  return [loadedResToElmData(savedData, treeId), [], conflictsExist, savedMetadata];
}

async function pull(localDb, remoteDb, treeId, source) {
  let selector = { _id: { $regex: `${treeId}/` } };
  let results = await localDb.replicate.from(remoteDb, { selector })
    .on('change', async (change) => {
      let metadataDocs = getMetadataDocs(change);
      if (metadataDocs.length > 0) {
        await resolveMetadataConflicts(localDb, metadataDocs);
      }
    })
    .catch(async (e) => e);

  if (results.ok && results.docs_written > 0) {
    return await load(localDb, treeId);
  } else {
    return null;
  }
}


async function push(localDb, remoteDb, treeId, checkForConflicts, successHandler) {
  let shouldPush = true;
  if (checkForConflicts) {
    let allDocs = await loadAll(localDb, treeId);
    let conflictedDocs = await getConflicts(localDb, allDocs, treeId);
    shouldPush = conflictedDocs.length === 0;
  }

  if (shouldPush) {
    let selector = { _id: { $regex: `${treeId}/` } };
    localDb.replicate.to(remoteDb, { selector })
      .on('complete', successHandler)
      .catch(async (e) => e);
  }
}


async function sync(localDb, remoteDb, treeId, conflictsExist, documentsReceivedHandler, pushSuccessHandler) {
  let dataAfterPull = await pull(localDb, remoteDb, treeId, "sync function");
  documentsReceivedHandler(dataAfterPull);

  if (typeof conflictsExist !== "boolean") {
    await push(localDb, remoteDb, treeId, true, pushSuccessHandler);
  } else if (!conflictsExist) {
    await push(localDb, remoteDb, treeId, false, pushSuccessHandler);
  }
}
/* === PRIVATE/INTERNAL === */

async function writeTree(workingTree) {
  let treeObjects = [];

  async function addShaIds(tree) {
    if (tree.children.length === 0) {
      let shaId = await sha1(tree.content+"\n");
      treeObjects.push({_id: shaId, type: "tree", content: tree.content, children: []});
      return Object.assign(tree, {_id: shaId });
    } else {
      let childrenWithShas = await Promise.all(tree.children.map(async t => await addShaIds(t)));
      let str = tree.content + "\n" + childrenWithShas.map(c => c._id + " " + c.id).join("\n");
      let shaId = await sha1(str);
      treeObjects.push({_id: shaId, type: "tree", content: tree.content, children: childrenWithShas.map(c => [c._id, c.id])});
      return Object.assign(tree, {_id: shaId });
    }
  }

  await addShaIds(workingTree);
  return treeObjects;
}


async function commitTree(author, parents, tree, timestamp, metadata) {
  let treeObjects = await writeTree(tree);
  let rootId = treeObjects[treeObjects.length - 1]._id;
  let str = rootId + "\n"
    + (parents.length === 1 ? parents[0] + "\n" : parents.join("\n"))
    + author + " " + (timestamp.toString());
  let commitSha = await sha1(str);
  let commitObj = {_id: commitSha, type: "commit", tree: rootId, parents: parents, author: author , timestamp: timestamp};
  return [commitSha, treeObjects.concat([commitObj, metadata])];
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


async function getConflicts (db, docsList, treeId) {
  let idConfTupleMap = (id, confRevs) => {
    return confRevs.flatMap(confRev => db.get(id, {rev: confRev}));
  }
  let getConflictPromises =
    docsList
      .filter(d => d.hasOwnProperty("_conflicts"))
      .map(d => [d._id, d._conflicts])
      .flatMap(tup => idConfTupleMap(tup[0],tup[1]))

  let resolved = await Promise.allSettled(getConflictPromises);

  return resolved
    .filter(p => p.status === "fulfilled")
    .map(p => p.value)
    .map(d => typeof treeId === "undefined" ? d : unprefix(d, treeId));
}


function maybeAddConflict(elmDocs, conflictingDocs, savedLocalHead) {
  let headId = `heads/master`;
  if  (  conflictingDocs.length === 0
      || typeof savedLocalHead === "undefined"
      || typeof conflictingDocs.find(d => d._id === headId) === "undefined"
      )
  { return elmDocs }

  let newDocs = Object.assign({}, elmDocs);

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


async function resolveMetadataConflicts (localDb, metadataDocs) {
  let docIds = metadataDocs.map(d => d._id);
  let options = {include_docs: true, conflicts: true, keys: docIds};
  let docsWithConflicts =
    (await localDb.allDocs(options))
      .rows
      .map(r => r.doc)
      .filter(d => d.hasOwnProperty("_conflicts"));
  let conflicts = await getConflicts(localDb, docsWithConflicts);

  let docsAndConflicts = docsWithConflicts.concat(conflicts);
  let chosen = _
    .chain(docsAndConflicts)
    .sortBy('updatedAt')
    .reverse()
    .uniqBy('docId')
    .value()
    .map(d => {return {id: d._id, rev: d._rev}})
  let all = docsAndConflicts.map(d => {return {id: d._id, rev: d._rev}});
  let losers = _.differenceBy(all, chosen, 'rev')

  let loserDelPromises = losers.map(d => localDb.remove(d.id, d.rev));
  await Promise.allSettled(loserDelPromises);
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


function getMetadataDocs (change) {
  return change.docs.filter(d => d._id.endsWith("metadata"));
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

export { getDocumentList, load, loadMetadata, saveData, newSave, pull, sync };