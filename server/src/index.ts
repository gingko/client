//@ts-strict-ignore
// Node.js
import fs from "node:fs";
import crypto from "node:crypto";
import { Buffer } from 'node:buffer';

// Databases
import Nano from "nano";
import Database from 'better-sqlite3'
import { createClient } from "redis";

// Networking & Server
import express from "express";
import proxy from "express-http-proxy";
import session from "express-session";
import redisConnect from 'connect-redis';
import { WebSocketServer } from "ws";
import * as realtime from "./realtime.js";
import axios from "axios";
import FormData from "form-data";
import Mailgun from "mailgun.js";
import config from "../config.js";
import Stripe from 'stripe';

// Misc
import _ from "lodash";
import {expand, compact, SnapshotCompaction, debounce} from './snapshots.js';
import nodePandoc from "node-pandoc";
import URLSafeBase64 from "urlsafe-base64";
import * as uuid from "uuid";
import hlc from "@tpp/hybrid-logical-clock";
import Debug from "debug";
import * as ai from "./ai.js";
const debug = Debug('cards');
const aiDebug = Debug('ai');
import morgan from "morgan";




/* ==== SQLite3 ==== */

const db = new Database('../data/data.sqlite');
db.pragma('journal_mode = WAL');

// Litestream Recommendations
db.pragma('busy_timeout = 5000');
db.pragma('synchronous = NORMAL');

// Create Tables
db.exec('CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, salt TEXT, password TEXT, createdAt INTEGER, confirmedAt INTEGER, paymentStatus TEXT, language TEXT)');
db.exec('CREATE TABLE IF NOT EXISTS resetTokens (token TEXT PRIMARY KEY, email TEXT, createdAt INTEGER)');
db.exec('CREATE TABLE IF NOT EXISTS trees (id TEXT PRIMARY KEY, name TEXT, location TEXT, owner TEXT, collaborators TEXT, inviteUrl TEXT, createdAt INTEGER, updatedAt INTEGER, deletedAt INTEGER, migratedTo TEXT, publicUrl TEXT)');
db.exec(`CREATE TABLE IF NOT EXISTS tree_collaborators ( tree_id TEXT, user_id TEXT, FOREIGN KEY(tree_id) REFERENCES trees(id) ON DELETE CASCADE, FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE, UNIQUE(tree_id, user_id) ) `);
db.exec('CREATE TABLE IF NOT EXISTS cards (id TEXT PRIMARY KEY, treeId TEXT, content TEXT, parentId TEXT, position FLOAT, updatedAt TEXT, deleted BOOLEAN)');
db.exec('CREATE INDEX IF NOT EXISTS cards_treeId ON cards (treeId)');
db.exec('CREATE TABLE IF NOT EXISTS tree_snapshots ( snapshot TEXT, treeId TEXT, id TEXT, content TEXT, parentId TEXT, position REAL, updatedAt TEXT, delta BOOLEAN)')
db.exec('CREATE INDEX IF NOT EXISTS tree_snapshots_treeId ON tree_snapshots (treeId)');
db.exec('CREATE TABLE IF NOT EXISTS feature_flags (user_id TEXT, feature TEXT, enabled BOOLEAN, PRIMARY KEY (user_id, feature), FOREIGN KEY(user_id) REFERENCES users(id) ON DELETE CASCADE)');
db.exec('CREATE TABLE IF NOT EXISTS feature_rollouts (feature TEXT PRIMARY KEY, percent REAL NOT NULL CHECK (percent >= 0 AND percent <= 100) )');

// Users Table
const userByEmail = db.prepare('SELECT * FROM users WHERE id = ?');
const userByRowId = db.prepare('SELECT * FROM users WHERE rowid = ?');
const userSignup = db.prepare('INSERT INTO users (id, salt, password, createdAt, confirmedAt, paymentStatus, language) VALUES (?, ?, ?, ?, ?, ?, ?)');
const userConfirm = db.prepare('UPDATE users SET confirmedAt = ? WHERE id = ?');
const userChangePassword = db.prepare('UPDATE users SET salt = ?, password = ? WHERE id = ?');
const userSetLanguage = db.prepare('UPDATE users SET language = ? WHERE id = ?');
const userSetPaymentStatus = db.prepare('UPDATE users SET paymentStatus = ? WHERE id = ?');
const expireTestUser = db.prepare("UPDATE users SET paymentStatus='trial:' || CAST(1000*(unixepoch() - 2*24*60*60) AS TEXT) WHERE id = 'cypress@testing.com'");

// Reset Token Table
const resetToken = db.prepare('SELECT * FROM resetTokens WHERE token = ?');
const resetTokenInsert = db.prepare('INSERT INTO resetTokens (token, email, createdAt) VALUES (?, ?, ?)');
const resetTokenDelete = db.prepare('DELETE FROM resetTokens WHERE email = ?');

// Trees Table
const treesByOwner = db.prepare('SELECT * FROM trees WHERE owner = ?');
const treeOwner = db.prepare('SELECT owner FROM trees WHERE id = ?').pluck();
const treeById = db.prepare('SELECT * FROM trees WHERE id = ?');
const treeByPublicUrl = db.prepare('SELECT * FROM trees WHERE publicUrl = ?');
const treesModdedBeforeWithSnapshots = db.prepare('SELECT DISTINCT t.id FROM trees t JOIN tree_snapshots ts ON t.id = ts.treeId WHERE ts.delta = 0 AND t.updatedAt < ? ORDER BY t.updatedAt ASC');
const treeUpsert = db.prepare(`
INSERT INTO trees(id, name, location, owner, inviteUrl, publicUrl, createdAt, updatedAt, deletedAt)
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
ON CONFLICT(id) DO UPDATE SET
  name = excluded.name,
  location = excluded.location,
  owner = excluded.owner,
  inviteUrl = excluded.inviteUrl,
  publicUrl = excluded.publicUrl,
  updatedAt = excluded.updatedAt,
  deletedAt = excluded.deletedAt;
`);
const upsertMany = db.transaction((requesterId, treesRecvd) => {
    for (const treeRecvd of treesRecvd) {
      const treeInDb = treeById.get(treeRecvd.id);
      if (treeInDb && treeInDb.owner !== treeRecvd.owner) {
        console.error(`User ${requesterId} attempted to update tree ${treeRecvd.id} owned by ${treeInDb.owner}`);
        continue;
      }

      treeUpsert.run(treeRecvd.id, treeRecvd.name, treeRecvd.location, treeRecvd.owner, treeRecvd.inviteUrl, treeRecvd.publicUrl, treeRecvd.createdAt, treeRecvd.updatedAt, treeRecvd.deletedAt);
    }
});

// Collaborators Table
const treeCollaboratorsInsert = db.prepare('INSERT OR REPLACE INTO tree_collaborators (tree_id, user_id) VALUES (?, ?)');
const treeCollaboratorsDelete = db.prepare('DELETE FROM tree_collaborators WHERE tree_id = ? AND user_id = ?');
const treeCollaboratorsByTree = db.prepare('SELECT user_id FROM tree_collaborators WHERE tree_id = ?').pluck();
const treeIdsSharedWithUser = db.prepare('SELECT tree_id FROM tree_collaborators WHERE user_id = ?').pluck();
const treesSharedWithUser = db.prepare('SELECT * FROM trees WHERE id IN (SELECT tree_id FROM tree_collaborators WHERE user_id = ?)');

// Cards Table
const cardsSince = db.prepare('SELECT * FROM cards WHERE treeId = ? AND updatedAt > ? ORDER BY updatedAt ASC');
const cardsAllUndeleted = db.prepare('SELECT * FROM cards WHERE treeId = ? AND deleted = FALSE ORDER BY updatedAt ASC');
const cardById = db.prepare('SELECT * FROM cards WHERE id = ?');
const cardInsert = db.prepare('INSERT OR REPLACE INTO cards (updatedAt, id, treeId, content, parentId, position, deleted) VALUES (?, ?, ?, ?, ?, ?, ?)');
const cardUpdate = db.prepare('UPDATE cards SET updatedAt = ?, content = ? WHERE id = ?');
const cardUpdateTs = db.prepare('UPDATE cards SET updatedAt = ? WHERE id = ?');
const cardMove = db.prepare('UPDATE cards SET updatedAt = ?, parentId = ?, position = ? WHERE id = ?');
const cardDelete = db.prepare('UPDATE cards SET updatedAt = ?, deleted = TRUE WHERE id = ?');
const cardUndelete = db.prepare('UPDATE cards SET deleted = FALSE WHERE id = ?');

// Tree Snapshots Table
const takeSnapshotSQL = db.prepare(`
INSERT INTO tree_snapshots (snapshot, treeId, id, content, parentId, position, updatedAt, delta)
SELECT
 (SELECT substr(updatedAt, 1, instr(updatedAt, ':') - 1) as updatedAtTime 
  FROM cards WHERE treeId = @treeId AND deleted != 1 ORDER BY updatedAtTime DESC LIMIT 1
 ) || \':\' || treeId, treeId, id, content, parentId, position, updatedAt, 0
FROM cards WHERE treeId = @treeId AND deleted != 1
`);
const getSnapshots = db.prepare('SELECT * FROM tree_snapshots WHERE treeId = ? ORDER BY snapshot ASC');
const getSnapshotIdsUncompacted = db.prepare('SELECT DISTINCT snapshot FROM tree_snapshots WHERE treeId = ? AND delta = 0 ORDER BY updatedAt DESC');
const treeIdsWithUncompactedSnapshots = db.prepare('SELECT DISTINCT treeId FROM tree_snapshots WHERE delta = 0').pluck();
const filterSnapshots = db.prepare('DELETE FROM tree_snapshots WHERE treeId = ? AND snapshot NOT IN (SELECT value FROM json_each(?))');
const removeSnapshot = db.prepare('DELETE FROM tree_snapshots WHERE snapshot = ? AND treeId = ?');
const insertSnapshotDeltaRow = db.prepare('INSERT INTO tree_snapshots (snapshot, treeId, id, content, parentId, position, updatedAt, delta) VALUES (@snapshot, @treeId, @id, @content, @parentId, @position, @updatedAt, 1);');
const runCompactions = db.transaction((compactions : SnapshotCompaction[]) => {
  for(const compaction of compactions) {
    removeSnapshot.run(compaction.snapshot, compaction.treeId);
    for (const row of compaction.compactedData) {
      insertSnapshotDeltaRow.run(row);
    }
  }
});
//@ts-ignore
const compactTreesTx = db.transaction((treeIds : string[]) => {
  for (const treeId of treeIds) {
    debug(`Compacting tree ${treeId}`)
    const snapshots = getSnapshots.all(treeId);
    if (snapshots.length > 0) {
      const compactions = compact(snapshots);
      if (compactions.length > 0) {
        debug(`Compacting ${compactions.length} snapshots for tree ${treeId}`)
        runCompactions(compactions);
        debug(`Compacted ${compactions.length} snapshots for tree ${treeId}`)
      }
    }
  }
})

const compactAllBefore = function(timestamp : number) {
  const treeIds = treesModdedBeforeWithSnapshots.all(timestamp).map((row) => row.id);
  debug(`Compacting ${treeIds.length} trees`)
  if (treeIds.length > 0) {
    compactTreesTx.immediate(treeIds);
  }
}


_.mixin({
  memoizeDebounce: function(func, wait=0, options={}) {
    var mem = _.memoize(function() {
      return _.debounce(func, wait, options)
    }, options.resolver);
    return function(){mem.apply(this, arguments).apply(this, arguments)}
  }
});
//@ts-ignore
const takeSnapshotDebounced = _.memoizeDebounce((treeId) => {
  debug(`Taking snapshot for tree ${treeId}`)
    takeSnapshotSQL.run({treeId});
} , /* 6 hour debounce */ 6 * 60 * 60 * 1000
  , { leading: true, trailing: false }
);

// Feature Flags Table
const featureFlagsByUser = db.prepare('SELECT feature FROM feature_flags WHERE user_id = ? AND enabled = TRUE').pluck();

// Feature Rollouts Table
const featureRollouts = db.prepare('SELECT * FROM feature_rollouts');

function getHashedPercentage(email: string, feature: string) {
  const hash = crypto.createHash('sha256');
  hash.update(email + feature);
  const hashHex = hash.digest('hex');
  // Use the first 8 characters of the hash (32 bits) to get a float
  const hashInt = parseInt(hashHex.slice(0, 8), 16);
  const percent = (hashInt / 0xffffffff) * 100; // Convert to percentage
  console.log(`Hashed percentage for ${email} and ${feature} is ${percent}`);
  return percent;
}

function getEnabledFeaturesForUser(userId : string) {
  const enabledFeatures = featureFlagsByUser.all(userId);
  const rolloutFeatures = featureRollouts.all().filter(f => getHashedPercentage(userId, f.feature) <= f.percent).map(f => f.feature);
  const featureSet = new Set([...enabledFeatures, ...rolloutFeatures]);
  return Array.from(featureSet);
}


/* ==== SETUP ==== */

const nano = Nano(`http://${config.COUCHDB_USER}:${config.COUCHDB_PASS}@127.0.0.1:5984`);

const app = express();
const port = process.env.PORT || 3000;

// Use morgan to log requests in immediate mode:
app.use(morgan(':remote-addr - :remote-user [:date[clf]] ":method :url HTTP/:http-version"', {"immediate": true}));
// Use morgan to log responses separately:
app.use(morgan(':remote-addr - :remote-user [:date[clf]] ":method :url HTTP/:http-version" :status :response-time ms'));

app.use(express.json({limit: '50mb'}));
app.use(express.urlencoded({ extended: true }));

const mailgun = new Mailgun(FormData);
const mg = mailgun.client({
  username: "api",
  key: config.MAILGUN_API_KEY,
});


/* ==== Start Server ==== */

const server = app.listen(port, () => console.log(`Example app listening at https://localhost:${port}`));

// Session

const RedisStore = redisConnect(session);
const redis = createClient({legacyMode: true});

const sessionParser = session({
    store: new RedisStore({ client: redis }),
    secret: config.SESSION_SECRET,
    resave: false, // required: force lightweight session keep alive (touch)
    saveUninitialized: false, // recommended: don't save empty sessions
    cookie: { secure: false, maxAge: /* 6 months */ 6 * 30 * 24 * 60 * 60 * 1000 }
});
redis.connect().catch(console.error);

redis.on("error", function (err) {
  console.error("Redis Error " + err);
});
redis.on("connect", function () {
  console.log("Redis connected");
});
app.use(sessionParser);

/* ==== WebSocket ==== */

function heartbeat() {
  this.isAlive = true;
}

const wss = new WebSocketServer({noServer: true});
const wsToUser = new Map();
const userToWs = new Map<string, Set<WebSocket>>();
const channels = new Map<string, realtime.User[]>();

wss.on('connection', (ws, req) => {
  ws.isAlive = true;
  ws.on('pong', heartbeat);
  console.log("ws connection", req.session);
  const userId = req.session.user;
  wsToUser.set(ws, userId);

  // Add ws to user's entry in userToWs
  if (userToWs.has(userId)) {
    userToWs.get(userId).add(ws);
  } else {
    userToWs.set(userId, new Set([ws]));
  }

  const userDataUnsafe = userByEmail.get(userId);
  if (userDataUnsafe && userDataUnsafe.paymentStatus) {
    const userData = _.omit(userDataUnsafe, ['salt', 'password']);
    userData.features = getEnabledFeaturesForUser(userId);
    ws.send(JSON.stringify({t: "user", d: userData}));
  }

  const userTrees = treesByOwner.all(userId);
  const sharedWithUserTrees = treesSharedWithUser.all(userId);
  ws.send(JSON.stringify({t: "trees", d: withCollaboratorArrays([...userTrees, ...sharedWithUserTrees])}));

  ws.on('message', async function incoming(message) {
    try {
      if (message == 'ping') {
        ws.send('pong');
        return;
      }

      const msg = JSON.parse(message);
      switch (msg.t) {
        case "trees": {
          upsertMany(userId, msg.d);
          ws.send(JSON.stringify({t: "treesOk", d: msg.d.sort((a, b) => a.createdAt - b.createdAt)[0].updatedAt}));
          const treeIds = msg.d.map(t => t.id);

          // send a message to all users in the tree channels, except for the user who sent the message
          for (const [treeId, users] of channels) {
            if (treeIds.includes(treeId)) {
              for (const u of users) {
                if (u.ws !== ws) {
                  u.ws.send(JSON.stringify({t: 'trees', d: withCollaboratorArrays([treeById.get(treeId)])}));
                }
              }
            }
          }
          break;
        }

        case 'pull':
          if (msg.d[1] == '0') {
            const cards = cardsAllUndeleted.all(msg.d[0]);
            ws.send(JSON.stringify({t: 'cards', d: cards}));
          } else {
            const cards = cardsSince.all(msg.d[0], msg.d[1]);
            ws.send(JSON.stringify({t: 'cards', d: cards}));
          }
          break;

        case 'push':
          // No need for permissions check, as the conflict resolution will take care of it
          const lastTsRecvd = msg.d.dlts[msg.d.dlts.length - 1].ts;
          debug('push recvd ts: ', lastTsRecvd)
          const treeId = msg.d.tr;

          // Check if the tree is owned by the user or shared with the user
          const tree = treeById.get(treeId);
          const treeCollaborators = treeCollaboratorsByTree.all(treeId);
          if (tree.owner !== userId && !treeCollaborators.includes(userId)) {
            ws.send(JSON.stringify({t: 'pushError', d: 'You do not have permission to edit this tree'}));
            break;
          }

          // Note : If I'm not generating any hybrid logical clock values,
          // then having this here is likely pointless.
          hlc.recv(lastTsRecvd);

          const savedTs = [];
          const deltasTx = db.transaction(() => {
            for (let delta of msg.d.dlts) {
              let savedTsInDelta = runDelta(treeId, delta, userId)
              savedTs.push(...savedTsInDelta);
            }
          });
          try {
            deltasTx.immediate();
            takeSnapshotDebounced(treeId);

            if (savedTs.length === 0) {
              throw new Error('Transaction passed but no cards saved');
            }

            debug('pushOk : ', savedTs);

            ws.send(JSON.stringify({t: 'pushOk', d: savedTs}));

            const owner = treeOwner.get(treeId);
            const collaborators = treeCollaboratorsByTree.all(treeId);
            const usersToNotify = [owner, ...collaborators];
            for (const [otherWs, userId] of wsToUser) {
              if (usersToNotify.includes(userId) && otherWs !== ws) {
                otherWs.send(JSON.stringify({t: "doPull", d: treeId}));
              }
            }
          } catch (e) {
            if (e instanceof ConflictError) {
              const cards = cardsSince.all(msg.d.tr, msg.d.chk);
              console.error(e, cards);
              if (cards.length === 0 && e.conflict) {
                cards.push(e.conflict);
              }
              ws.send(JSON.stringify({t: 'cardsConflict', d: cards, e: e }));
            } else {
              ws.send(JSON.stringify({t: 'pushError', d: e}));
              axios.post(config.NTFY_URL, e.message).catch(e => console.error(e));
              console.error(e);
            }
            debug(e.message)
          }
          break;

        case 'pullHistoryMeta': {
          const treeId = msg.d;
          const history = getSnapshots.all(treeId);
          const historyMeta = _.chain(history)
            .groupBy('snapshot')
            .mapValues(s => ({id: s[0].snapshot, ts: s[0].snapshot}))
            .values()
            .value();
          ws.send(JSON.stringify({t: 'historyMeta', d: historyMeta, tr: treeId}));
          break;
        }

        case 'pullHistory': {
          const treeId = msg.d;
          const history = getSnapshots.all(treeId);
          const expandedHistory = expand(history);
          const historyData = _.chain(expandedHistory)
            .groupBy('snapshot')
            .mapValues(s => ({id: s[0].snapshot, ts: s[0].snapshot, d: s}))
            .values()
            .value();
          ws.send(JSON.stringify({t: 'history', d: historyData, tr: treeId}));
          break;
        }

        case 'setLanguage':
          userSetLanguage.run(msg.d, userId);
          ws.send(JSON.stringify({t: 'userSettingOk', d: ['language', msg.d]}));
          break;

        case 'rt:addCollab': {
          const collabEmail = msg.d.c;
          const treeId = msg.d.tr;
          const collabTree = treeById.get(treeId);
          if (collabTree.owner !== userId) {
            ws.send(JSON.stringify({t: 'collab.addError', d: 'Only the owner can add collaborators'}));
            break;
          }
          try {
            treeCollaboratorsInsert.run(treeId, collabEmail);
            ws.send(JSON.stringify({t: 'trees', d: withCollaboratorArrays([treeById.get(treeId)])}));

            // Get the collaborator's ws and send them the tree with the new collaborator array
            const collabWsSet = userToWs.get(collabEmail);
            if (collabWsSet) {
              for (const collabWs of collabWsSet) {
                collabWs.send(JSON.stringify({t: 'trees', d: withCollaboratorArrays([treeById.get(treeId)])}));
              }
            }

          } catch (e) {
            if (e.code && e.code === "SQLITE_CONSTRAINT_FOREIGNKEY") {
              ws.send(JSON.stringify({t: 'collab.addError', d: 'User does not exist'}));
            } else {
              console.error(e);
            }
          }
          break;
        }

        case 'rt:removeCollab': {
          const collabEmail = msg.d.c;
          const treeId = msg.d.tr;
          const collabTree = treeById.get(treeId);
          if (collabTree.owner !== userId) {
            ws.send(JSON.stringify({t: 'collab.removeError', d: 'Only the owner can remove collaborators'}));
            break;
          }
          treeCollaboratorsDelete.run(treeId, collabEmail);
          ws.send(JSON.stringify({t: 'trees', d: withCollaboratorArrays([treeById.get(treeId)])}));

          // Get the collaborator's ws and send them the tree with the new collaborator array
          const collabWsSet = userToWs.get(collabEmail);
          if (collabWsSet) {
            for (const collabWs of collabWsSet) {
              collabWs.send(JSON.stringify({t: 'removedFrom', d: treeId }));
            }
          }
          break;
        }

        case 'rt': {
          realtime.handleRT(channels, userId, msg.d);
          break;
        }

        case 'rt:join': {
          realtime.join(channels, userId, ws, msg.d);
          break;
        }

        case 'ai:generate-new': {
          const prompt = msg.d;
          aiDebug('ai:generate-new', prompt);
          try {
            const res = await ai.newDocument(prompt);

            ws.send(JSON.stringify({t: 'ai:generate-new', d: res}));
          } catch (e) {
            console.error(e);
          }
          break;
        }

        case 'ai:generate-children': {
          // Get the content, treeId, and cardId
          const cardId = msg.d.id;
          const prompt = msg.d.prompt;
          const card = cardById.get(cardId);
          aiDebug('ai:generate-children', card, prompt);
          try {
            const res = await ai.insertChildren(prompt);

            db.transaction(() => {
              res.forEach((content, i) => {
                const newCardId = uuid.v4();
                const ts = hlc.nxt();
                aiDebug('ai:generate-children', newCardId, card.treeId, content, cardId, ts);
                cardInsert.run(ts, newCardId, card.treeId, content, cardId, i, 0);
              })
            }).immediate();

            ws.send(JSON.stringify({t: 'ai:success', d: {t: card.treeId, i: cardId}}));
          } catch (e) {
            console.error(e);
          }
          break;
        }

        case 'ai:generate-below': {
          // Get the content, treeId, and cardId
          const cardId = msg.d.id;
          const prompt = msg.d.prompt;
          const card = cardById.get(cardId);
          aiDebug('ai:generate-below', card, prompt);
          try {
            const res = await ai.insertBelow(prompt);

            db.transaction(() => {
              res.forEach((content, i) => {
                const newCardId = uuid.v4();
                const ts = hlc.nxt();
                aiDebug('ai:generate-below', newCardId, card.treeId, content, cardId, ts);
                cardInsert.run(ts, newCardId, card.treeId, content, card.parentId, card.position + i, 0);
              })
            }).immediate();

            ws.send(JSON.stringify({t: 'ai:success', d: {t: card.treeId, i: cardId}}));
          } catch (e) {
            console.error(e);
          }
          break;
        }


        default:
          console.error('Unknown message type: ', msg.t)
          break;
      }
    } catch (e) {
      console.error(e);
    }
  });

  ws.on('error', (err) => {
    console.error(err);
  });

  ws.on('close', () => {
    wsToUser.delete(ws);

    const userWsSet = userToWs.get(userId);
    if (userWsSet) {
      userWsSet.delete(ws);
      if (userWsSet.size === 0) {
        userToWs.delete(userId);
      }
    }

    realtime.disconnectWebSocket(channels, ws);
  });
});

const interval = setInterval(function ping() {
  wss.clients.forEach(function each(ws) {
    if (ws.isAlive === false) return ws.terminate();

    ws.isAlive = false;
    ws.ping();
  });
}, 30000);

wss.on('close', function close() {
  clearInterval(interval);
});

wss.on('error', (err) => {
  console.error(err);
});


app.head('/session', (req, res) => {
  if (req.session) {
    if (req.session.user) {
      res.status(200).send();
    } else {
      // Session found without user field
      console.error('Session user not found:', req.session)
      req.session.destroy((err) => {
        if(err) { console.log(err); }
        res.status(401).send();
      });
    }
  } else {
    res.status(401).send();
  }
});

server.on('upgrade', async (request, socket, head) => {
  sessionParser(request, {}, (err) => {
    if (err) {
      console.error('Session retrieval error:', err);
      socket.write('HTTP/1.1 401 Unauthorized\r\n\r\n');
      socket.destroy();
      return;
    }

    if (request.session && request.session.user) {
      wss.handleUpgrade(request, socket, head, (ws) => {
        wss.emit('connection', ws, request);
      });
    } else {
      if (request.session) {
        console.error('Session user not found:', request.session)
      } else {
        console.error('Session not found');
      }
      socket.write('HTTP/1.1 403 Forbidden\r\n\r\n');
      socket.destroy();
      return;
    }
  });
});



/* ==== Authentication ==== */

const iterations = 10;
const keylen = 20;
const encoding = 'hex';
const digest = 'SHA1';

app.post('/signup', async (req, res) => {
  const email = req.body.email.toLowerCase();
  const password = req.body.password;
  let didSubscribe = req.body.subscribed;
  let userDbName = `userdb-${toHex(email)}`;
  const timestamp = Date.now();
  const confirmTime = timestamp; //Until we re-implement welcome emails and newsletters, don't show 'confirm your email' message
  const trialExpiry = timestamp + 14*24*3600*1000;

  const salt = crypto.randomBytes(16).toString('hex');
  let hash = crypto.pbkdf2Sync(password, salt, iterations, keylen, digest).toString(encoding);
  try {
    let userInsertInfo = userSignup.run(email, salt, hash, timestamp, confirmTime, "trial:" + trialExpiry, "en");
    const user = userByRowId.get(userInsertInfo.lastInsertRowid);

    req.session.user = email;

    // Calling save manually to generate CouchDB user
    req.session.save(async (err) => {
      if(err) {
        console.error(err);
        res.status(500).send({error: "Internal server error at signup"});
      }

      await nano.db.create(userDbName);

      //@ts-ignore
      await nano.request({db: userDbName, method: 'put', path: '/_security', body: {members: {names: [email], roles: []}}});

      let data = _.omit(user, ['id', 'email', 'password', 'salt']);
      data.email = user.id;

      res.status(200).send(data);
    })
  } catch (e) {
    if (e.code && e.code === "SQLITE_CONSTRAINT_PRIMARYKEY") {
      console.error(e);
      res.status(409).send();
    } else {
      console.error(e);
      res.status(500).send({error: "Internal server error"});
    }
  }
});


app.post('/login', async (req, res) => {
  let email = req.body.email.toLowerCase();
  let password = req.body.password;

  // Check SQLite DB for user and password
  let user = userByEmail.get(email);

  if (user !== undefined) {
    crypto.pbkdf2(password, user.salt, iterations, keylen, digest, (err, derivedKey) => {
        if (err) throw err;
        if (derivedKey.toString(encoding) === user.password) {
          // Authentication successful
          doLogin(req, res, user);
        } else {
          res.status(401).send();
        }
    });
  } else {
    // User not found
    res.status(401).send();
  }
});

function doLogin(req, res, user) {
  req.session.user = user.id;

  req.session.save((err) => {
    if(err) {
      res.status(500).send({error: "Internal server error at doLogin"});
      console.error(err);
    }

    const userTrees = treesByOwner.all(user.id);
    const sharedWithUserTrees = treesSharedWithUser.all(user.id);
    const allTrees = [...userTrees, ...sharedWithUserTrees];
    let data = _.omit(user, ['id', 'email', 'password', 'salt']);
    data.email = user.id;
    data.documents = withCollaboratorArrays(allTrees);

    res.status(200).send(data);
  });
}


app.post('/logout', async (req, res) => {
  if (req.session) {
    req.session.destroy((err) => {
      if(err) { console.log(err); }
      res.clearCookie("connect.sid").status(200).send();
    });
  } else {
    res.status(200).send();
  }
});


app.post('/forgot-password', async (req, res) => {
  let email = req.body.email;
  try {
    let user = userByEmail.get(email);

    if (!user) {
      res.status(404).send();
      return;
    }

    let token = newToken();
    user.resetToken = hashToken(token);
    user.tokenCreatedAt = Date.now();

    resetTokenInsert.run(user.resetToken, email, user.tokenCreatedAt);

    const msg = {
      from: config.SUPPORT_EMAIL,
      to: [email],
      subject: 'Password Reset link for Gingkowriter.com',
      text: `The reset link: https://app.gingkowriter.com/reset-password/${token}`,
      html: `The reset link: https://app.gingkowriter.com/reset-password/${token}`
    }

    await mg.messages.create(config.MAILGUN_DOMAIN, msg);
    res.status(200).send({email: email})
  } catch (err) {
    console.error(err);
    res.status(err.statusCode).send();
  }
});


app.post('/reset-password', async (req, res) => {
  let token = req.body.token;
  let newPassword = req.body.password;

  try {
    let tokenRow = resetToken.get(hashToken(token));
    if (!tokenRow) {
      res.status(404).send();
      return;
    }

    let timeElapsed = Date.now() - tokenRow.createdAt;
    if (timeElapsed < 3600000) {
        let user = userByEmail.get(tokenRow.email);
        if (user) {
            const salt = crypto.randomBytes(16).toString('hex');
            let hash = crypto.pbkdf2Sync(newPassword, salt, iterations, keylen, digest).toString(encoding);
            userChangePassword.run(salt, hash, user.id);
            const updatedUser = userByEmail.get(tokenRow.email);
            doLogin(req, res, updatedUser);
        } else {
            res.status(404).send();
        }
    } else {
      res.status(403).send();
    }

    // Whether the token is expired or not, delete it from the database
    resetTokenDelete.run(tokenRow.email);
  } catch (err) {
    console.error(err)
    res.status(err.response.status).send(err.response.data);
  }
});


/* ==== Public Pages ==== */

app.get('/public/:page', (req, res) => {
  if (!(req.subdomains[0] === 'public')) {
    res.status(404).send();
    return;
  }

  const rawTree = treeByPublicUrl.get(req.params.page);

  if (!rawTree) {
    res.status(404).send();
    return;
  }

  const tree = _.pick(rawTree, ['id', 'name', 'createdAt', 'updatedAt']);
  const cards = cardsAllUndeleted.all(tree.id);
  res.send({tree, cards});
});



/* ==== DB proxy ==== */

app.use('/db', proxy('http://127.0.0.1:5984', {
  proxyReqOptDecorator: function(proxyReqOpts, srcReq) {
    if (srcReq.session.user) {
      proxyReqOpts.headers['X-Auth-CouchDB-UserName'] = srcReq.session.user;
      proxyReqOpts.headers['X-Auth-CouchDB-Roles'] = '';
    } else {
      //console.log('No user in session for /db', srcReq);
    }
    return proxyReqOpts;
  }
}));


/* ==== Contact Us Route ==== */

app.post('/pleasenospam', async (req, res) => {
  const msg = {
    from: config.SUPPORT_EMAIL,
    to: [req.body.toEmail],
    'h:Reply-To': req.body.fromEmail,
    cc: [req.body.fromEmail],
    subject: req.body.subject,
    text: req.body.body,
    html: req.body.body,
  }

  const urgentAutoresponse = {
    from: config.SUPPORT_URGENT_EMAIL,
    to: [req.body.fromEmail],
    subject: config.URGENT_MESSAGE_SUBJECT,
    html: config.URGENT_MESSAGE_BODY,
  }

  try {
    await mg.messages.create(config.MAILGUN_DOMAIN, msg);

    if (req.body.toEmail == config.SUPPORT_URGENT_EMAIL) {
      await mg.messages.create(config.MAILGUN_DOMAIN, urgentAutoresponse);
    }

    res.status(201).send();
  } catch (err) {
    console.error(err)
    res.status(err.code || 400).send(err);
  }
});



/* ==== Payment ==== */

const stripe = new Stripe(config.STRIPE_SECRET_KEY, { apiVersion: '2022-11-15', typescript: true });

app.post('/create-checkout-session', async (req, res) => {
  const { priceId, customer_email } = req.body;

  try {
    // @ts-ignore : docs say to remove 'payment_method_types' but typescript complains
    const stripeSession = await stripe.checkout.sessions.create({
      mode: "subscription",
      line_items: [
        {
          price: priceId,
          quantity: 1,
        },
      ],
      allow_promotion_codes: true,
      customer_email: customer_email,
      success_url: config.URL_ROOT + '/upgrade/success?session_id={CHECKOUT_SESSION_ID}',
      cancel_url: config.URL_ROOT,
    });

    res.send({
      sessionId: stripeSession.id,
    });
  } catch (e) {
    console.error(e);
    res.status(400);
    return res.send({
      error: {
        message: e.message,
      }
    });
  }
});


app.post('/create-portal-session', async (req, res) => {
  const { customer_id } = req.body;

  const stripeSession = await stripe.billingPortal.sessions.create({
    customer: customer_id
  });

  res.redirect(stripeSession.url);
});


app.post('/hooks', async (req, res) => {
  let event = req.body;

  // Handle the event
  switch (event.type) {
    case 'checkout.session.completed':
      // Get data from event
      let email = event.data.object.customer_email;
      let custId = event.data.object.customer;
      userSetPaymentStatus.run("customer:" + custId, email);
      sendUpdatedUserData(email);
      break;
    // ... handle other event types
    default:
      console.log(`Unhandled event type ${event.type}`);
  }

  // Return a res to acknowledge receipt of the event
  res.json({received: true});
});




/* ==== Mail confirmation ==== */

let confirmedHandler = (email) => {
  userConfirm.run(Date.now(), email);
  sendUpdatedUserData(email);
};

app.post('/mlhooks', async (req, res) => {
  let events = req.body.events;


  // Handle the events
  let subscribers = events.map(x => x.data.subscriber);

  subscribers.filter(s => s.confirmation_timestamp).map(s => {
    if (s.confirmation_timestamp) {
      confirmedHandler(s.email);
    }
  });

  // Return a res to acknowledge receipt of the event
  res.json({received: true});
});




/* ==== Export ==== */

app.post('/export-docx', async (req, res) => {
  // receive Markdown string, return file download of docx
  let srcFile = `./${req.body.docId}.tmp.md`;
  let outFile = `${req.body.docId}.docx`
  res.header('Content-Type', 'application/octet-stream; charset=utf-8');

  fs.writeFile(srcFile, req.body.markdown, () => {
    let args =['-f', 'markdown', '-t', 'docx', '-o', outFile]
    nodePandoc(srcFile, args, () => {
      fs.createReadStream(outFile).pipe(res);
    })
  });
});



/* ==== Testing ==== */

app.delete('/test/user', async (req, res) => {
  let userDbName = `userdb-${toHex("cypress@testing.com")}`;

  try {
    await nano.db.destroy(userDbName).catch(e => null);
    res.status(200).send();
  } catch (err) {
    console.error(err);
    res.status(500).send(err);
  }
});

app.post('/test/expired', async (req, res) => {
  try {
    expireTestUser.run();
    sendUpdatedUserData("cypress@testing.com");
    res.status(200).send();
  } catch (err) {
    console.error(err);
    res.status(500).send(err);
  }
});

app.post('/test/trees', async (req, res) => {
  const trees = req.body;
  try {
    upsertMany("cypress@testing.com", trees);
    res.status(200).send();
  } catch (err) {
    console.error(err);
    res.status(500).send(err);
  }
});

app.post('/test/confirm', async (req, res) => {
  try {
    confirmedHandler("cypress@testing.com")
    res.status(200).send();
  } catch (err) {
    console.error(err);
    res.status(500).send(err);
  }
});




/* ==== Utils ==== */

app.get('/utils/compact', (req, res) => {
  if (req.hostname === 'localhost' || req.hostname === '127.0.0.1') {
    const daysAgo = req.query.daysAgo;
    const timestamp = Date.now() - (daysAgo * 24 * 60 * 60 * 1000);
    debug(`Compacting all trees before ${timestamp}`);
    compactAllBefore(timestamp);
    res.send("Compacting");
  } else {
    res.status(403).send("Forbidden");
  }
});


app.get('/utils/debounce/all', (req, res) => {
  if (req.hostname === 'localhost' || req.hostname === '127.0.0.1') {
    const hours = req.query.h || 0;
    const seconds = req.query.s || 0;
    if (hours === 0 && seconds === 0) {
      res.status(400).send("Must specify hours or seconds");
      return;
    }
    const debounceMs = (hours * 60 * 60 * 1000) + (seconds * 1000);
    const treeIdsToDebounce = treeIdsWithUncompactedSnapshots.all();
    console.log('treeIdsToDebounce: ', treeIdsToDebounce);
    db.transaction(() => {
      for (const treeId of treeIdsToDebounce) {
        runFilterSnapshots(treeId, debounceMs);
      }
    }).immediate();

    res.send("Debounced");
  }
});


app.get('/utils/debounce/:treeId', (req, res) => {
  if (req.hostname === 'localhost' || req.hostname === '127.0.0.1') {
    const hours = req.query.h || 0;
    const seconds = req.query.s || 0;
    if (hours === 0 && seconds === 0) {
      res.status(400).send("Must specify hours or seconds");
      return;
    }
    const debounceMs = (hours * 60 * 60 * 1000) + (seconds * 1000);
    const treeId = req.params.treeId;

    runFilterSnapshots(treeId, debounceMs);
    res.send("Debounced");
  }
});


function runFilterSnapshots(treeId, debounceMs) {
  const snapshots = getSnapshotIdsUncompacted.all(treeId);

  const snapshotsToKeep = debounce(snapshots, debounceMs);
  console.log('snapshotsToKeep: ', snapshotsToKeep)
  console.log(`Debounced tree ${treeId} to ${snapshotsToKeep.length} snapshots`);

  const resultString = JSON.stringify(snapshotsToKeep.map(s => s.snapshot));
  console.log('resultString: ', resultString)
  filterSnapshots.run(treeId, resultString)
}


/* ==== Static ==== */

app.use(express.static("../client/web"));






/* ==== Single Page App ==== */

// Respond to all non-file requests with index.html
app.get('*', (req, res) => {
  if (req.session) {
    req.session.lastAccessed = Date.now();
  }
  const index = new URL('../../client/web/index.html', import.meta.url).pathname;
  res.sendFile(index);
});


/* ==== Delta Handlers ==== */

class ConflictError extends Error {
  conflict: any;

  constructor(message?: string, conflict?: any) {
    super(message); // pass the message up to the Error constructor

    // Set the prototype explicitly to allow instanceof checks to work correctly
    // since TypeScript doesn't set the prototype automatically when extending native JavaScript classes
    Object.setPrototypeOf(this, ConflictError.prototype);

    // This line is necessary to get the correct stack trace
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, ConflictError);
    }

    this.conflict = conflict;

    this.name = 'ConflictError'; // custom name for your error
  }
}

function runDelta(treeId, delta, userId) : string[] {
  const ts = delta.ts;
  const savedTs = [];

  if (delta.ops.length === 0) {
    savedTs.push(runUpdTs(ts, delta.id))
    return savedTs;
  }

  for (let op of delta.ops) {
    switch (op.t) {
      case 'i':
        savedTs.push(runIns(ts, treeId, userId, delta.id, op));
        break;

      case 'u':
        savedTs.push(runUpd(ts, delta.id, op));
        break;

      case 'm':
        savedTs.push(runMov(ts, delta.id, op));
        break;

      case 'd':
        savedTs.push(runDel(ts, delta.id, op));
        break;

      case 'ud':
        savedTs.push(runUndel(ts, delta.id));
        break;
    }
  }
  return savedTs;
}

function runIns(ts, treeId, userId, id, ins ) : string  {
  // To prevent insertion of cards to trees the user shouldn't have access to
  let treesOwned = treesByOwner.all(userId).map(t => t.id);
  const sharedWithUserTrees = treeIdsSharedWithUser.all(userId);
  console.log('sharedWithUserTrees: ', sharedWithUserTrees);
  const userTrees = [...treesOwned, ...sharedWithUserTrees];
  if (!userTrees.includes(treeId)) {
    throw new ConflictError(`User ${userId} doesn't have access to tree ${treeId}`);
  }

  const parentPresent = ins.p == null || cardById.get(ins.p);
  if (parentPresent) {
    cardInsert.run(ts, id, treeId, ins.c, ins.p, ins.pos, 0);
    debug(`${ts}: Inserted card ${id.slice(0,10)} at ${ins.p ? ins.p.slice(0,10) : ins.p} with ${JSON.stringify(ins.c.slice(0, 20))}`);
    return ts;
  } else {
    throw new ConflictError(`Ins Conflict : Parent ${ins.p} not present`);
  }
}

function runUpd(ts, id, upd ) : string {
  const card = cardById.get(id);
  if (card != null /*&& card.updatedAt == upd.e*/) {
    // card is present (disable timestamp check, for LWW mode)
    cardUpdate.run(ts, upd.c, id);
    debug(`${ts}: Updated card ${id} to ${JSON.stringify(upd.c.slice(0,20))}`);
    return ts;
  } else if (card == null) {
    throw new ConflictError(`Upd Conflict : Card '${id}' not present.`);
  } else {
    throw new ConflictError(`Upd Conflict : Card '${id}' unknown error`);
  }
}

function runMov(ts, id, mov ) : string  {
  const parentPresent = mov.p == null || cardById.get(mov.p) != null;
  const card = cardById.get(id);
  if (card != null && parentPresent && !isAncestor(id, mov.p)) {
    cardMove.run(ts, mov.p, mov.pos, id);
    debug(`${ts}: Moved card ${id} to ${mov.p} at ${mov.pos}`);
    return ts;
  } else if(card == null) {
    throw new ConflictError(`Mov Conflict : Card ${id} not present`);
  } else if (!parentPresent) {
    throw new ConflictError(`Mov Conflict : Parent ${mov.p} not present`);
  } else if (isAncestor(id, mov.p)) {
    throw new ConflictError(`Mov Conflict : Card ${id} is an ancestor of ${mov.p}`);
  } else {
    throw new ConflictError(`Mov Conflict : Card ${id} unknown error`);
  }
}

function runDel(ts, id, del ) : string {
  const card = cardById.get(id);
  if (card != null && card.updatedAt == del.e) {
    cardDelete.run(ts, id);
    debug(`${ts}: Deleted card ${id}`);
    return ts;
  } else if (card == null) {
    throw new ConflictError(`Del Conflict : Card '${id}' not present`);
  } else if (card.updatedAt != del.e) {
    let msg = `Del Conflict : Card '${id}' timestamp mismatch : ${card.updatedAt} != ${del.e}`;
    throw new ConflictError(msg, card);
  } else {
    throw new ConflictError(`Del Conflict : Card '${id}' unknown error`);
  }
}

function runUndel(ts, id) : string {
  const info = cardUndelete.run(id);
  if (info.changes == 0) {
    throw new ConflictError('Undel Conflict : Card not present');
  }
  debug(`${ts}: Undeleted card ${id}`);
  return ts;
}

function runUpdTs(ts, id) : string {
  const info = cardUpdateTs.run(ts, id);
  if (info.changes == 0) {
    throw new ConflictError('UpdTs Conflict : Card not present');
  }
  debug(`${ts}: Updated card ${id} timestamp to ${ts}`);
  return ts;
}

// --- Helpers ---


function isAncestor(cardId , targetParentId ) {
  if (targetParentId == null) {
    return false;
  } else if (cardId == targetParentId) {
    return false;
  } else {
    const parent = cardById.get(targetParentId);
    return isAncestor(cardId, parent.parentId);
  }
}


/* === HELPERS === */

function sendUpdatedUserData(email) {
  const userDataUnsafe = userByEmail.get(email);
  const userData = _.omit(userDataUnsafe, ['salt', 'password']);
  const userWebSockets = userToWs.get(email);

  if (userWebSockets) {
    userWebSockets.forEach(ws => {
      ws.send(JSON.stringify({ t: "user", d: userData}));
    })
  }
}

function withCollaboratorArrays(trees) {
  return trees.map(tree => {
    const collaborators = treeCollaboratorsByTree.all(tree.id);
    return {...tree, collaborators};
  })
}

function toHex(str) {
  return Buffer.from(str).toString('hex');
}


function newToken() {
 return URLSafeBase64.encode(uuid.v4(null, new Buffer(16)));
}


function hashToken(token) {
  return crypto.createHash('sha256').update(token).digest('hex');
}
