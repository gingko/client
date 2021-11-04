const config = require("../../config.js");

describe('Welcome Tree & Checklist', () => {
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

  it('Has working Welcome tree and checklist', () => {
    cy.url().as('welcomeTreeUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('#title', 'welcome')

    cy.get('#welcome-to-this-example-gingko-tree-')
      .should('be.visible')
      .should('contain', 'Welcome to this example Gingko tree')

    // Welcome checklist is visible and none are done
    cy.get('#welcome-checklist-container')
      .should('be.visible')
      .find('ul li')
      .should('not.have.class', 'done')

    // Shortcut for moving triggers "done" on nav-with-arrows
    cy.shortcut('{rightArrow}')

    cy.get('#welcome-checklist-container li.nav-with-arrows')
      .should('have.class', 'done')

    // Using mouse should not count as success
    cy.get('.card-btn.edit').click()
    cy.writeInCard('k')
    cy.get('.card-btn').click()

    cy.get('#welcome-checklist-container li.edit-with-keyboard')
      .should('not.have.class', 'done')
    cy.get('#welcome-checklist-container li.save-with-keyboard')
      .should('not.have.class', 'done')

    // Edit with shortcut checklist item
    cy.shortcut('{enter}')

    cy.get('#welcome-checklist-container li.edit-with-keyboard')
      .should('have.class', 'done')

    cy.writeInCard('s')

    // Save with shortcut checklist item
    cy.shortcut('{ctrl}{enter}')

    cy.get('#welcome-checklist-container li.save-with-keyboard')
      .should('have.class', 'done')

    // Create via + button click shouldn't count as success
    cy.get('.ins-right').click()

    cy.get('#welcome-checklist-container li.create-with-keyboard')
      .should('not.have.class', 'done')

    cy.shortcut('{esc}')

    // Create below with shortcut checklist item
    cy.shortcut('{ctrl}{downArrow}')

    cy.get('#welcome-checklist-container li.create-with-keyboard')
      .should('have.class', 'done')

    cy.get('#welcome-checklist-container li.create-child-with-keyboard')
      .should('not.have.class', 'done')

    cy.shortcut('{esc}')

    // Create below with shortcut checklist item
    cy.shortcut('{ctrl}{rightArrow}')

    cy.get('#welcome-checklist-container li.create-child-with-keyboard')
      .should('have.class', 'done')

    cy.shortcut('{esc}')
    cy.shortcut('{leftArrow}')
    cy.shortcut('{leftArrow}')
    cy.wait(300)

    // Drag card checklist item
    // (since drag-drop in Cypress is tricky, we cheat by using keyboard
    // and marking all card moves as success )
    cy.shortcut('{alt}{downArrow}')

    cy.get('#welcome-checklist-container li.drag-card')
      .should('have.class', 'done')

    // Shouldn't show welcome checklist on reload
    cy.get('@welcomeTreeUrl').then((url) => {
      cy.visit(url)

      cy.url().should('eq', url)

      cy.contains('#title', 'welcome')

      cy.get('#welcome-checklist-container')
        .should('not.exist')
    })

  })
})
