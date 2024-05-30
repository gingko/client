import { Db, ObjectId, Document, MongoClient, ServerApiVersion, WithId } from "mongodb";
import * as hiddenConfig from '../../hidden-config.js';

// SQLite setup
import { Database } from 'bun:sqlite';
const sqliteDb = new Database(hiddenConfig.IMPORT_TEST_SQLITE_DB_PATH);

const createTreesTable = sqliteDb.query(`CREATE TABLE IF NOT EXISTS trees (id TEXT PRIMARY KEY, name TEXT, location TEXT, owner TEXT, collaborators TEXT, inviteUrl TEXT, createdAt INTEGER, updatedAt INTEGER, deletedAt INTEGER, publicUrl TEXT, migratedTo TEXT);`)
const createCardsTable = sqliteDb.query(`CREATE TABLE IF NOT EXISTS cards (id TEXT PRIMARY KEY, treeId TEXT, content TEXT, parentId TEXT, position FLOAT, updatedAt TEXT, deleted BOOLEAN)`);
createTreesTable.run();
createCardsTable.run();

const insertTree = sqliteDb.query(`INSERT INTO trees (id, name, location, owner, collaborators, inviteUrl, createdAt, updatedAt, deletedAt, publicUrl, migratedTo) VALUES ($id, $name, 'cardbased', $owner, '[]', NULL, $createdAt, $updatedAt, NULL, NULL, NULL)`);
const insertCard = sqliteDb.query(`INSERT INTO cards (id, treeId, content, parentId, position, updatedAt) VALUES ($id, $treeId, $content, $parentId, $position, $updatedAt)`);
const insertCards = sqliteDb.transaction( cards => {
  for (const card of cards) {
    insertCard.run(card);
  }
});


// MongoDB setup
const client = new MongoClient(hiddenConfig.LEGACY_MONGODB_URI, {
  serverApi: ServerApiVersion.v1,
});

// Elm setup

const {Elm} = require('./LegacyWorker.js')

const app = Elm.LegacyWorker.init();

// Main function

async function main() {
  try {
    await client.connect();
    console.log('Connected to the server');

    const db = client.db('gingko');

    switch (Bun.argv[2]) {
      case 'getTreesByName' : {
        const trees = await db.collection('trees').find({ name: Bun.argv[3] }).toArray();
        console.log(trees.map(tree => [tree._id, tree.name]).join('\n'));
        return;
      }

      case 'getTreeById' : {
        const tree = await db.collection('trees').findOne({ _id: new ObjectId(Bun.argv[3]) });
        console.log(tree);
        return;
      }

      case 'getUserById' : {
        const user = await db.collection('users').findOne({ _id: new ObjectId(Bun.argv[3]) });
        console.log(user);
        return;
      }

      case 'getUserTreesByEmail': {
        const trees = await getUserTreesByEmail(db, Bun.argv[3]);
        console.log(trees.map(tree => [tree._id, tree.name]).join('\n'));
        return;
      }

      case 'convertUserTreesByIds': {
        if (Bun.argv.length < 5 || Bun.argv[4].split(',').length === 0) {
          console.error('Usage: bun mongodb-to-sqlite.ts convertUserTreesByIds <emailLegacy> <treeId1,treeId2,...>');
          return;
        }

        const emailLegacy = Bun.argv[3];

        const treeIds = Bun.argv[4].split(',');
        console.log('Getting trees:', treeIds);
        const treesToConvert = await getUserTreesByIds(db, emailLegacy, treeIds);

        if (treesToConvert.length === 0) {
          console.error('No trees to convert');
          return;
        }

        for (const tree of treesToConvert) {
          console.log('Converting tree:', tree._id);
          await convertTree(db, emailLegacy, tree);
        }
      }
    }
  } finally {
    await client.close();
  }
}

async function getUserTreesByIds(db : Db, emailLegacy: string, treeIds: string[]) {
  if (!db) {
    throw new Error('Database not connected');
  }

  const user = await db.collection('users').findOne({ email: emailLegacy });
  if (!user) {
    throw new Error('User not found');
  }

  return await db.collection('trees').find({ owner: user._id, _id: { $in: treeIds.map(id => new ObjectId(id)) } }).toArray();
}

async function getUserTreesByEmail(db : Db, emailLegacy: string) {
  if (!db) {
    throw new Error('Database not connected');
  }

  const user = await db.collection('users').findOne({ email: emailLegacy });
  if (!user) {
    throw new Error('User not found');
  }

  return await db.collection('trees').find({ owner: user._id }).toArray();
}


async function convertTree(db: Db, owner: string, tree : WithId<Document>) {
  if (!db) {
    throw new Error('Database not connected');
  }

  // Insert tree
  const treeToInsert = treeMongoToSqlite(tree);
  treeToInsert.$owner = owner; // For now assuming email in the new version is same as in the legacy version
  insertTree.run(treeToInsert);

  // Get cards with treeId and deleted field null or not present
  const cards = await db.collection('cards').find({ treeId: tree._id, deleted: { $in: [null, false] } }).toArray();
  const cardsSqlite = cards.map(cardMongoToSqlite);
  console.log('Cards:', cardsSqlite.map(card => card.$content));
  //insertCards(cardsSqlite);
  return cardsSqlite;
}

function treeMongoToSqlite(tree) {
  return {
    $id: tree._id.toHexString(),
    $name: tree.name,
    $owner: tree.owner.toHexString(),
    $createdAt: tree.createdAt.toISOString(),
    $updatedAt: tree.updatedAt.toISOString(),
  };
}

function cardMongoToSqlite(card) {
  return {
    $id: card._id.toHexString(),
    $treeId: card.treeId.toHexString(),
    $content: card.content,
    $parentId: card.parentId ? card.parentId.toHexString() : null,
    $position: card.position,
    $updatedAt: card.updatedAt.toISOString(),
  };
}


async function saveTreeAsJSON(db: Db, folderName: string, tree: WithId<Document>) {
  // Get cards with treeId and deleted field null or not present
  const cards = await db.collection('cards').find({ treeId: tree._id, deleted: { $in: [null, false] } }).toArray();
}


main().catch(console.error);