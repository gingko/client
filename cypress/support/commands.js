const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


Cypress.Commands.add('deleteUser', (userEmail)=> {
  cy.clearCookie('AuthSession')
  cy.request(
    { url: config.TEST_SERVER + '/db/_users/org.couchdb.user:'+userEmail
      , method: 'GET'
      , auth: {user: config.COUCHDB_ADMIN_USERNAME, password: config.COUCHDB_ADMIN_PASSWORD}
      , failOnStatusCode: false
    })
    .then((response) => {
      cy.log('delU', JSON.stringify(response))

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


Cypress.Commands.add('login', (userEmail) => {
  cy.request(
    { url: config.TEST_SERVER + '/login'
      , method: 'POST'
      , body: {email: userEmail, password: 'testing'}
    })
    .then((response) => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({email: userEmail, language: 'en'}))
    })
})


Cypress.Commands.add('writeInCard', (textToType) => {
  cy.get('textarea')
    .type(textToType)
})


Cypress.Commands.add('shortcut', (keys) => {
  cy.get('body').type(keys)
})