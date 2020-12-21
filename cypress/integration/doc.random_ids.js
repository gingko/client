const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


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

  it('Should not duplicate ids', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])

    cy.contains('Another Child card')
      .should('be.visible')
      .click()

    cy.shortcut('{ctrl}{downarrow}')
    cy.writeInCard('mod')
    cy.shortcut('{ctrl}{enter}')

    cy.contains('#save-indicator', 'Synced')

    // Switch to other tree and back
    cy.shortcut('{ctrl}o')
    cy.get('#switcher-modal input').type('ano')
    cy.shortcut('{enter}')
    cy.shortcut('{ctrl}o')
    cy.get('#switcher-modal input').type('unt')
    cy.shortcut('{enter}')


    cy.contains('mod')
      .should('be.visible')
      .click()

    // Check for one textarea only
    cy.shortcut('{ctrl}{downarrow}')
    cy.get('textarea')
      .should('have.length', 1)
  })
})
