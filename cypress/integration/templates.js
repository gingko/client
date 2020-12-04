const config = require("../../config.js");

describe('Welcome Tree & Templates', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
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

  it('Starts with the Welcome Tree after signup', () => {
    cy.get('#welcome-to-gingko-writer')
      .should('be.visible')
      .should('contain', 'Welcome to Gingko Writer')
  })

  it('Should bring up the Template Selector on clicking "New"', () => {
    cy.get('#file-button').click()

    cy.get('#new-button').click()

    cy.get('#template-new')
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')

    cy.get('#template-timeline')
      .should('be.visible')

    cy.get('#template-academic')
      .should('be.visible')
  })

  it('Should show all templates on narrow screens', () => {
    cy.viewport(600,900)

    cy.get('#template-new')
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')

    cy.get('#template-timeline')
      .should('be.visible')

    cy.get('#template-academic')
      .should('be.visible')
  })
})
