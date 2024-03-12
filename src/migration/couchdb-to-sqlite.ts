import * as data from '../shared/data.js';
import * as hiddenConfig from '../../hidden-config.js';
import { Database } from 'bun:sqlite';
import PouchDB from "pouchdb";

const db = new Database(hiddenConfig.SQLITE_DB_PATH);
console.log(db);
const couchdbTreesByUser = db.query(`SELECT * FROM trees WHERE location='couchdb' AND owner = ?`);
const insertCard = db.query(`INSERT INTO cards (id, treeId, content, parentId, position, updatedAt, deleted) VALUES ($id, $treeId, $content, $parentId, $position, $updatedAt, $deleted)`);
const insertCards = db.transaction( cards => {
  for (const card of cards) {
    insertCard.run(card);
  }
})
const setTreeLocation = db.query(`UPDATE trees SET location='cardbased' WHERE id = ?`);

/* ==== Elm Setup ==== */

const {Elm} = require('./MigrationWorker.js')

const app = Elm.MigrationWorker.init();

app.ports.output.subscribe(function([docId, data]) {
  try {
    console.log('Received conversion data for ', docId);
    insertCards(data.map(cardToQuery));
    setTreeLocation.run(docId);
  } catch (e) {
    console.error(e);
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
  }

  // get first treeId
  const treeId = treesToConvert[0];

  // Load tree from CouchDB
  const [treeData, rest] = await data.load(couchdb, treeId);

  app.ports.input.send([treeId, treeData]);
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

function cardToQuery(card) {
  return {
    $id: card.id,
    $treeId: card.treeId,
    $content: card.content,
    $parentId: card.parentId,
    $position: card.position,
    $updatedAt: card.updatedAt,
    $deleted: false
  }
}