const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


describe('Not Found (Logged In User)', () => {
  const testEmail = 'cypress@testing.com'
  const testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup_with(testEmail, 'twoTrees')
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should redirect to last updated tree', function () {
    cy.visit(config.TEST_SERVER+ '/aaaaa')

    cy.url().should('eq', config.TEST_SERVER + '/' + this.treeIds[1])
  })
})
