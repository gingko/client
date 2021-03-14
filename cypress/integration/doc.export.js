const config = require("../../config.js");

describe('Document Exporting', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'sevenTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('sevenTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Exports correctly', function () {
    // Romeo & Juliet tree
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.url().should('contain', this.treeIds[0] )

    // Select a mid-column card
    cy.getCard(2,1,4)
      .should('contain.html', '<h2 id="act-1-scene-3">Act 1, Scene 3</h2>')
      .click()
      .should('have.class','active')

    // Try various export options (Preview Only)
    cy.get('#export-button').click()
    cy.get('#export-preview-checkbox').check()

    describe('Rendered/Word format', () => {
      describe('Whole tree', () => {
        cy.get('#export-preview', {log: false})
          .should('contain.html', '<h1 id="act-i">Act I</h1>')
          .should('contain.html', '<h2 id="act-1-prologue">Act 1, Prologue</h2>')
      })
    })
  })
})
