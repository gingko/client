import { Db, ObjectId, MongoClient, ServerApiVersion } from "mongodb";
import * as hiddenConfig from '../../hidden-config.js';

// SQLite setup
import { Database } from 'bun:sqlite';
const sqliteDb = new Database(hiddenConfig.IMPORT_TEST_SQLITE_DB_PATH);
const createCardsTable = sqliteDb.query(`CREATE TABLE IF NOT EXISTS cards (id TEXT PRIMARY KEY, treeId TEXT, content TEXT, parentId TEXT, position FLOAT, updatedAt TEXT, deleted BOOLEAN)`);
createCardsTable.run();
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


// Main function

async function main() {
  try {
    await client.connect();
    console.log('Connected to the server');

    const db = client.db('gingko');

    switch (Bun.argv[2]) {
      case 'getUserTreesByEmail': {
        const trees = await getUserTreesByEmail(db, Bun.argv[3]);
        console.log(trees.map(tree => [tree._id, tree.name]).join('\n'));
      }

      case 'convertUserTreesByIds': {
        if (Bun.argv.length < 5 || Bun.argv[4].split(',').length === 0) {
          console.error('Usage: bun mongodb-to-sqlite.ts convertUserTreesByIds <email> <treeId1,treeId2,...>');
          return;
        }

        const treeIds = Bun.argv[4].split(',');
        console.log('Getting trees:', treeIds);
        const treesToConvert = await getUserTreesByIds(db, Bun.argv[3], treeIds);

        if (treesToConvert.length === 0) {
          console.error('No trees to convert');
          return;
        }

        for (const tree of treesToConvert) {
          console.log('Converting tree:', tree._id);
          await convertTree(db, tree._id);
        }
      }
    }
  } finally {
    await client.close();
  }
}

async function getUserTreesByIds(db : Db, email: string, treeIds: string[]) {
  if (!db) {
    throw new Error('Database not connected');
  }

  const user = await db.collection('users').findOne({ email });
  if (!user) {
    throw new Error('User not found');
  }

  return await db.collection('trees').find({ owner: user._id, _id: { $in: treeIds.map(id => new ObjectId(id)) } }).toArray();
}

async function getUserTreesByEmail(db : Db, email: string) {
  if (!db) {
    throw new Error('Database not connected');
  }

  const user = await db.collection('users').findOne({ email });
  if (!user) {
    throw new Error('User not found');
  }

  return await db.collection('trees').find({ owner: user._id }).toArray();
}


async function convertTree(db: Db, treeId: ObjectId) {
  if (!db) {
    throw new Error('Database not connected');
  }

  // Get cards with treeId and deleted field null or not present
  const cards = await db.collection('cards').find({ treeId, deleted: { $in: [null, false] } }).toArray();
  const cardsSqlite = cards.map(cardMongoToSqlite);
  console.log('Cards:', cardsSqlite.length);
  insertCards(cardsSqlite);
  return cardsSqlite;
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

main().catch(console.error);