const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");

Cypress.on('log:changed', options => {
  if (options.instrument === 'command' && options.consoleProps) {
    options.wallClockStoppedAt = Date.now()
    options.duration = +options.wallClockStoppedAt - (+ new Date(options.wallClockStartedAt))
    options.consoleProps.Duration = options.duration
  }
})

Cypress.Commands.add('deleteUser', (userEmail)=> {
  cy.clearCookie('AuthSession')
  cy.request('POST', config.TEST_SERVER + '/logout')
  cy.request(
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

  indexedDB.databases().then((dbs) => {
    dbs.filter((db) => db.name.includes(helpers.toHex(userEmail)))
      .map((db) => { window.indexedDB.deleteDatabase(db.name); })
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


Cypress.Commands.add('signup_blank', (userEmail) => {
  let body =
    { '_id': `org.couchdb.user:${userEmail}`
    , type: "user"
    , roles: []
    , name: userEmail
    , password: 'testing'
    };

  cy.request(
    { url: config.COUCHDB_SERVER + '/_users'
    , method: 'POST'
    , body: body
    , 'auth': {
        'user': config.COUCHDB_ADMIN_USERNAME,
        'pass': config.COUCHDB_ADMIN_PASSWORD,
      }
    })
})


Cypress.Commands.add('login', (userEmail) => {
  cy.request(
    { url: config.TEST_SERVER + '/login'
    , method: 'POST'
    , body: {email: userEmail, password: 'testing'}
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