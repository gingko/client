const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");
const _ = require("lodash");
const data = require("../../src/shared/data.js")
import PouchDB from "pouchdb";


describe('Managing Documents', () => {
  const testEmail = 'cypress@testing.com'
  const testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup_blank(testEmail)

    cy.task('db:seed', { dbName: testUserDb, seedName: 'twoTrees' })

    cy.request('POST', config.TEST_SERVER + '/logout')
    cy.clearCookie('AuthSession')

    cy.login(testEmail)
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Loads correct data into local PouchDB', function () {
    let currTreeId = this.treeIds[0];
    cy.visit(config.TEST_SERVER + '/' + currTreeId)

    cy.contains('Synced').then(async () => {
      let db = new PouchDB(testUserDb);

      let [loadedData, savedIds] = await data.load(db, currTreeId);

      expect(loadedData)
        .to.have.keys(['commit', 'metadata', 'ref', 'tree'])
    })
  })

  it('Saves data with correct author info', () => {
    cy.shortcut('{enter}')
    cy.writeInCard('{enter}A change')
    cy.shortcut('{ctrl}{enter}')

    cy.window().then((win) => {
      let {elmMessage, elmData} = win.elmMessages.slice(-1)[0];

      expect(elmMessage).to.eq("SaveData")

      let lastCommit = _.sortBy(elmData.filter(d => d.type == 'commit'), 'timestamp').reverse()[0];
      expect(lastCommit.author)
        .to.eq(`<${testEmail}>`)
    })
  })
})

