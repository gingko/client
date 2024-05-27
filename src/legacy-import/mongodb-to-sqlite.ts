import { Db, ObjectId, MongoClient, ServerApiVersion } from "mongodb";
import * as hiddenConfig from '../../hidden-config.js';

const client = new MongoClient(hiddenConfig.LEGACY_MONGODB_URI, {
  serverApi: ServerApiVersion.v1,
});

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

        const allTrees = await getUserTreesByEmail(db, Bun.argv[3]);
        const treeIds = Bun.argv[4].split(',');

        const treesToConvert = allTrees.filter(tree => treeIds.includes(tree._id.toString()));

        if (treesToConvert.length === 0) {
          console.error('No trees to convert');
          return;
        }

        for (const tree of treesToConvert) {
          await convertTree(db, tree._id);
        }
      }
    }
  } finally {
    await client.close();
  }
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

  // Get cards for tree
  const cards = await db.collection('cards').find({ treeId }).toArray();
  console.log('Cards:', cards);
  return cards;
}

main().catch(console.error);