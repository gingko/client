const config = require("./config.js");
const nano = require('nano')(`http://${config.COUCHDB_USER}:${config.COUCHDB_PASS}@localhost:5984`);
const usersDB = nano.use("_users");
const v8 = require('v8');

function toHex(s) {
    // utf8 to latin1
    var s = unescape(encodeURIComponent(s));
    var h = "";
    for (var i = 0; i < s.length; i++) {
        h += s.charCodeAt(i).toString(16);
    }
    return h;
}


function structuredClone(obj) {
  return v8.deserialize(v8.serialize(obj));
};


async function getUsers(filterFn = () => true) {
  let userDocs = await usersDB.list({include_docs: true});
  let users =
    userDocs.rows
      .filter(x => x.id.startsWith("org.couchdb.user:"))
      .filter(filterFn)
  return users;
}


async function getDbIds(filterFn = () => true) {
  let users = await getUsers(filterFn);
  let userDbIds =
    users
      .map(u => "userdb-" + toHex(u.id.replace("org.couchdb.user:","")));
  return userDbIds;
}


async function modifySettingsDryRun(updateFn = (s) => s, userFilter = () => true, settingsFilter = () => true) {
  let userDbIds = await getDbIds(userFilter);
  let settingsPromises =
    userDbIds
      .map(async dbId => [dbId, await nano.request({db: dbId, doc: 'settings'}).catch(e => undefined)])
  let settings = await Promise.all(settingsPromises);
  let result =
    settings
      .filter(sArr => typeof sArr[1] !== "undefined")
      .filter(settingsFilter)
      .map(sArr => { return {db: sArr[0], body: updateFn(structuredClone(sArr[1])), old_body: sArr[1]} });
  return result;
}


async function modifySettings(dryRunResults) {
  if (!Array.isArray(dryRunResults)){
    return;
  }

  let settingsRequests =
    dryRunResults
      .map(reqObj=> nano.request({db: reqObj.db, method: "POST", body: reqObj.body}))

  return Promise.all(settingsRequests);
}
