const config = require("../../config.js");

describe('Welcome Tree & Templates', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.visit(config.TEST_SERVER)

      cy.get('#signup-email')
        .type(testEmail)

      cy.get('#signup-password')
        .type('testing')

      cy.get('button.cta')
        .click()
    })
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Has working Welcome tree and templates', () => {
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('#title', 'welcome')

    cy.get('#-welcome-to-gingko-writer')
      .should('exist')
      .should('contain', 'Welcome to Gingko Writer')


    // Should bring up the Template Selector on clicking "New"
    cy.get('#new-icon').click()

    cy.get('#template-new')
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')

    cy.get('#template-timeline')
      .scrollIntoView()
      .should('be.visible')

    // Should show all templates on narrow screens
    cy.viewport(600,900)

    cy.get('#template-new')
      .scrollIntoView()
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import-markdown')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')
  })
})
