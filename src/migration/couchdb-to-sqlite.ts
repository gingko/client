import * as data from '../shared/data.js';
import * as hiddenConfig from '../../hidden-config.js';
import { Database } from 'bun:sqlite';
import PouchDB from "pouchdb";
import sha1 from 'sha1';

const db = new Database(hiddenConfig.SQLITE_DB_PATH);
console.log(db);
const couchdbTreesByUser = db.query(`SELECT * FROM trees WHERE location='couchdb' AND deletedAt IS NULL AND owner = ?`);
const insertCard = db.query(`INSERT INTO cards (id, treeId, content, parentId, position, updatedAt, deleted) VALUES ($id, $treeId, $content, $parentId, $position, $updatedAt, $deleted)`);
const insertCards = db.transaction( cards => {
  for (const card of cards) {
    insertCard.run(card);
  }
})

// Query to copy the tree with id treeId to a new row with id newTreeId
const copyTree = db.query(`INSERT INTO trees
 (id, name, location, owner, collaborators, inviteUrl, createdAt, updatedAt, publicUrl)
 SELECT $newTreeId, name, 'cardbased', owner, collaborators, inviteUrl, createdAt, updatedAt, publicUrl
 FROM trees WHERE id = $treeId`);
const markAsMigrated = db.query(`UPDATE trees SET deletedAt = unixepoch() * 1000, migratedTo = $newTreeId WHERE id = $treeId`);

/* ==== Elm Setup ==== */

const {Elm} = require('./MigrationWorker.js')

const app = Elm.MigrationWorker.init();

app.ports.output.subscribe(function(dataReceived) {
  try {
    const [docId, data] = dataReceived;
    console.log('Received conversion data for ', docId);
    const newTreeId = randomString(7);
    console.log('New tree id:', newTreeId);

    const cardsToInsert = data.map(updateTreeId(newTreeId)).map(cardToQuery);
    console.log('card ids:', cardsToInsert.map(c => c.$id));

    copyTree.run({$newTreeId: newTreeId, $treeId: docId});
    markAsMigrated.run({$newTreeId: newTreeId, $treeId: docId});
    insertCards(cardsToInsert);
    console.log('Migration complete for ', docId);
  } catch (e) {
    console.error(e, dataReceived);
  }
});


/* ==== Run Migrations ==== */

async function setupDb(email) {
  const userDbName = `userdb-${toHex(email)}`;
  const userDbUrl = "http://localhost:5984/" + userDbName;
  const dbOpts = { skip_setup: true, auth: {
    username: hiddenConfig.COUCHDB_ADMIN_USERNAME,
    password: hiddenConfig.COUCHDB_ADMIN_PASSWORD
    } };
  const couchdb = new PouchDB(userDbUrl, dbOpts);

// Check couchdb exists and accessible before continuing
  let remoteDBinfo = await couchdb.info().catch((e) => {console.error(e)});

  if (!remoteDBinfo) {
    console.error('Remote DB not found or not accessible');
    return;
  } else {
    console.log('Remote DB found and accessible');
  }

  // Get list of trees from sqlite for user
  const treesToConvert = couchdbTreesByUser.all(email).map((row) => row.id);

  if (treesToConvert.length === 0) {
    console.error('No trees to convert');
    return;
  } else {
    console.log('Trees to convert:', treesToConvert);
  }

  for (const treeId of treesToConvert) {
    // Load tree from CouchDB
    const [treeData, rest] = await data.load(couchdb, treeId);

    app.ports.input.send([treeId, treeData]);
  }
}

if (Bun.argv.length == 3) {
  console.log('Running migration for', Bun.argv[2]);
  setupDb(Bun.argv[2]);
} else {
  console.error('Usage: bun couchdb-to-sqlite.ts <email>');
}




/* ==== Helpers ==== */

function toHex (str: string) {
  return Array.from(str).map(c => c.charCodeAt(0).toString(16)).join('');
}

function randomString(length) {
  const chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  let result = '';
  for (var i = length; i > 0; --i) result += chars[Math.floor(Math.random() * chars.length)];
  return result;
}

  function updateTreeId(newTreeId: string) {
  return function(card) {
    return {
      ...card,
      treeId: newTreeId
    };
  }
}

function cardToQuery(card) {
  const hashedId = sha1(card.treeId + card.id);
  const hashedParentId = card.parentId ? sha1(card.treeId + card.parentId) : null;
  return {
    $id: hashedId,
    $treeId: card.treeId,
    $content: card.content,
    $parentId: hashedParentId,
    $position: card.position,
    $updatedAt: card.updatedAt,
    $deleted: false
  }
}