const config = require("../../config.js");

describe('JSON Imports from Startup State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout')
    cy.deleteUser(testEmail)
    cy.visit(config.TEST_SERVER)

    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button.cta')
      .click()
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should bring up the Import Modal on clicking', () => {
    cy.get('#file-button').click()

    cy.get('#new-button').click()

    cy.get('#template-import')
      .click()
  })
})