const config = require("../../config.js");
const hiddenConfig = require("../../hidden-config.js");
const helpers = require("../../src/shared/doc-helpers.js");
import PouchDB from "pouchdb";

Cypress.on('log:changed', options => {
  if (options.instrument === 'command' && options.consoleProps) {
    options.wallClockStoppedAt = Date.now()
    options.duration = +options.wallClockStoppedAt - (+ new Date(options.wallClockStartedAt))
    options.consoleProps.Duration = options.duration
  }
})

const origLog = Cypress.log;
Cypress.log = function (opts, ...other) {
  if (opts.displayName === 'script' || opts.name === 'request') {
    return;
  }
  return origLog(opts, ...other);
};

Cypress.Commands.add('deleteUser', (userEmail)=> {
  const testUserDb = 'userdb-' + helpers.toHex(userEmail);
  if (typeof indexedDB.databases == 'function') {
    console.log('CHROME')
    indexedDB.databases().then((dbs) => {
      dbs.filter((db) => db.name.includes(testUserDb) || db.name == 'db')
        .map((db) => {window.indexedDB.deleteDatabase(db.name)})
    })
  } else {
    console.log('FIREFOX')
    let db = new PouchDB(testUserDb, {skip_setup: true});
    db.destroy();
  }
  cy.clearAllCookies();
  cy.request('POST', config.TEST_SERVER + '/logout')
  return cy.request(
    { url: config.TEST_SERVER + '/test/user'
    , method: 'DELETE'
    , failOnStatusCode: false
    })
    .then((resp) => {
      cy.task('db:user:delete')
    })
})


Cypress.Commands.add('signup', (userEmail) => {
  cy.request(
    { url: config.TEST_SERVER + '/signup'
      , method: 'POST'
      , body: {email: userEmail, password: 'testing'}
    })
    .then((response) => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({ email: userEmail, language: 'en' }))
    })
})


Cypress.Commands.add('signup_with', (userEmail, seedName) =>{
  cy.request(
    { url: config.TEST_SERVER + '/signup'
      , method: 'POST'
      , body: {email: userEmail, password: 'testing'}
    })
    .then((response) => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({ email: userEmail, language: 'en' }))
      cy.task('db:sqlite:seed',{seedName: seedName})
      cy.visit(config.TEST_SERVER)
      cy.wait(1000)
    })
})

Cypress.Commands.add('login', (userEmail) => {
  cy.request(
    { url: config.TEST_SERVER + '/login'
    , method: 'POST'
    , body: {email: userEmail, password: 'testing'}
    , retryOnStatusCodeFailure: true
    })
    .then((response) => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({"email": userEmail,"language":"en"}))
    })
})


Cypress.Commands.add('writeInCard', (textToType) => {
  cy.get('textarea')
    .type(textToType, {delay: 30})
})


Cypress.Commands.add('shortcut', (keys) => {
  cy.get('body').type(keys, { delay: 30 })
})


Cypress.Commands.add('getColumn', (colNum) => {
  cy.get(`#column-container > .column:nth-child(${colNum})`)
})


Cypress.Commands.add('getGroup', (colNum, groupNum) => {
  cy.get(`#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1})`)
})


Cypress.Commands.add('getCard', (colNum, groupNum, cardNum) => {
  cy.get(`#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1}) > .card:nth-child(${cardNum})`)
})