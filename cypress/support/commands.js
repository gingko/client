const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");
import PouchDB from "pouchdb";

Cypress.on('log:changed', options => {
  if (options.instrument === 'command' && options.consoleProps) {
    options.wallClockStoppedAt = Date.now()
    options.duration = +options.wallClockStoppedAt - (+ new Date(options.wallClockStartedAt))
    options.consoleProps.Duration = options.duration
  }
})

Cypress.Commands.add('deleteUser', (userEmail)=> {
  const testUserDb = 'userdb-' + helpers.toHex(userEmail);
  if (indexedDB.hasOwnProperty('databases')) {
    console.log('CHROME')
    indexedDB.databases().then((dbs) => {
      dbs.filter((db) => db.name.includes(testUserDb))
        .map((db) => {window.indexedDB.deleteDatabase(db.name)})
    })
  } else {
    console.log('FIREFOX')
    let db = new PouchDB(testUserDb, {skip_setup: true});
    db.destroy();
  }
  cy.clearCookie('AuthSession')
  cy.request('POST', config.TEST_SERVER + '/logout')
  return cy.request(
    { url: config.TEST_SERVER + '/db/_users/org.couchdb.user:'+userEmail
      , method: 'GET'
      , auth: {user: config.COUCHDB_ADMIN_USERNAME, password: config.COUCHDB_ADMIN_PASSWORD}
      , failOnStatusCode: false
    })
    .then((response) => {
      if(response.status === 200) {
        cy.request(
          { url: `${config.TEST_SERVER}/db/_users/org.couchdb.user:${userEmail}?rev=${response.body._rev}`
            , method: 'DELETE'
            , auth: {user: config.COUCHDB_ADMIN_USERNAME, password: config.COUCHDB_ADMIN_PASSWORD}
          })
      }
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
  const testUserDb = 'userdb-' + helpers.toHex(userEmail);
  cy.request(
    { url: config.TEST_SERVER + '/signup'
      , method: 'POST'
      , body: {email: userEmail, password: 'testing'}
    })
    .then((response) => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({ email: userEmail, language: 'en' }))
      cy.task('db:seed',{dbName: testUserDb, seedName: seedName})
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
    .type(textToType)
})


Cypress.Commands.add('shortcut', (keys) => {
  cy.get('body').type(keys)
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