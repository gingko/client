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

    cy.get('#welcome-to-this-example-gingko-tree-')
      .should('exist')
      .should('contain', 'Welcome to this example Gingko tree')

    cy.get('#welcome-checklist-container')
      .should('be.visible')
      .find('ul li')
      .should('not.have.class', 'done')


    // Should bring up the Template Selector on clicking "New"
    cy.get('#new-icon').click()

    cy.get('#template-new')
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')

    cy.get('#template-timeline')
      .should('be.visible')

    // Should show all templates on narrow screens
    cy.viewport(600,900)

    cy.get('#template-new')
      .should('be.visible')

    cy.get('#template-import-bulk')
      .should('be.visible')

    cy.get('#template-import-markdown')
      .should('be.visible')

    cy.get('#template-import')
      .should('be.visible')
  })
})
