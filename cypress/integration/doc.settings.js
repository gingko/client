const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}


describe('Loading indicators', () => {
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

  it('Can change the document language', () => {
    cy.visit(config.TEST_SERVER)

    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.get('#settings-button')
      .click()

    cy.contains('#sidebar-menu', 'Language')

    cy.get('#sidebar-menu select')
      .select("es")

    cy.contains('Sincronizado')
  })

  it('Persists the language setting on reload', () => {
    cy.visit(config.TEST_SERVER)
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.contains('Sincronizado')
  })
})
