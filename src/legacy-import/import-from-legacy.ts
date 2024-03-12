import { Db, MongoClient, ServerApiVersion } from "mongodb";
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

      case 'convertAllUserTrees': {
        const trees = await getUserTreesByEmail(db, Bun.argv[3]);
        console.log(trees.map(tree => [tree._id, tree.name]).join('\n'));
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

main().catch(console.error);