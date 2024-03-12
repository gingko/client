/* ==== Elm Setup ==== */

const {Elm} = require('./MigrationWorker.js')

const app = Elm.MigrationWorker.init();

console.log('Sending 5')
app.ports.input.send(5)

app.ports.output.subscribe(function(data) {
  console.log(data);
});


/* ==== Run Migrations ==== */

import * as data from '../shared/data.js';
import * as hiddenConfig from '../../hidden-config.js';
import { Database } from 'bun:sqlite';
import PouchDB from "pouchdb";

let remoteDB;
const sqlite = new Database(hiddenConfig.SQLITE_DB_PATH);
const couchdbTreesByUser = sqlite.query(`SELECT * FROM trees WHERE location='couchdb' AND owner = ?`);

async function setupDb(email) {
  const userDbName = `userdb-${toHex(email)}`;
  const userDbUrl = "http://localhost:5984/" + userDbName;
  const remoteOpts = { skip_setup: true, auth: {
    username: hiddenConfig.COUCHDB_ADMIN_USERNAME,
    password: hiddenConfig.COUCHDB_ADMIN_PASSWORD
    } };
  const remoteDB = new PouchDB(userDbUrl, remoteOpts);

// Check remoteDB exists and accessible before continuing
  let remoteDBinfo = await remoteDB.info().catch((e) => {console.error(e)});

  if (!remoteDBinfo) {
    console.error('Remote DB not found or not accessible');
    return;
  } else {
    console.log('Remote DB found and accessible');
  }

  // Get list of trees from sqlite for user
  const treesToConvert = await couchdbTreesByUser.all(email).map((row) => row.id);
  console.log(treesToConvert);

  // Load tree from CouchDB
  const treeData = await data.load(remoteDB, treesToConvert[0]);
  console.log(treeData[0]);
}


setupDb("xcv@testing.com");




/* ==== Helpers ==== */

function toHex (str: string) {
  return Array.from(str).map(c => c.charCodeAt(0).toString(16)).join('');
}