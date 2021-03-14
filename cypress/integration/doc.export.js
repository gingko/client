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

    describe('Whole Tree', () => {
      describe('Word format', () => {
        cy.get('#export-preview', {log: false})
          .should('contain.html', '<h1 id="act-i">Act I</h1>')
          .should('contain.html', '<h2 id="act-1-prologue">Act 1, Prologue</h2>')
          .should('contain.html', '<p><strong>Sampson</strong></p>\n<p>Gregory, o\' my word, we\'ll not carry coals.</p>')
      })

      describe('Plain text', () => {
        cy.get('#export-plain').click()
        cy.get('#export-preview pre', {log: false})
          .should('contain.text', '# Act I')
          .should('contain.text', '## Act 1, Prologue')
          .should('contain.text', '**Sampson**\n\nGregory, o\' my word, we\'ll not carry coals.')
      })

      describe('JSON format', () => {
        cy.get('#export-json').click()
        cy.get('#export-preview pre', {log: false})
          .should('contain.text', '"content": "# Act I')
          .should('contain.text', '"content": "## Act 1, Prologue')
          .should('contain.text', '"content": "**Sampson**\\n\\nGregory, o\' my word, we\'ll not carry coals.')
      })
    })
  })
})
