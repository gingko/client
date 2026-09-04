const config = require("../../config.js");

describe('Document Exporting', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'oneTree')
    })
  })

  beforeEach(() => {
    cy.fixture('oneTree.ids.json').as('treeIds')
  })

  it('Exports correctly', function () {
    // Romeo & Juliet tree
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.url().should('contain', this.treeIds[0] )
    cy.get('div.spinner', {timeout: 20000}).should('not.exist')

    cy.contains('Two noble families', {timeout: 20000})

    // Select a mid-column card
    cy.getCard(1,1,1)
      .should('have.class', 'active')

    cy.getCard(2,1,4)
      .should('contain.html', '<h2 id="act-1-scene-3">Act 1, Scene 3</h2>')
      .click()
      .should('have.class','active')

    // Try various export options (Preview Only)
    cy.get('#export-icon').click()

    describe('Whole Tree', () => {
      cy.get('#export-select-all').click()
      describe('Word format', () => {
        cy.get('#export-format-word').click()
        cy.get('#export-preview', {log: false})
          .should('contain.html', '<h1 id="act-i">Act I</h1>')
          .should('contain.html', '<h2 id="act-1-prologue">Act 1, Prologue</h2>')
          .should('contain.html', '<p><strong>Sampson</strong></p>\n<p>Gregory, o\' my word, we\'ll not carry coals.</p>')
      })

      describe('Plain text', () => {
        cy.get('#export-format-text').click()
        cy.get('#export-preview', {log: false})
          .should('contain.text', '# Act I')
          .should('contain.text', '## Act 1, Prologue')
          .should('contain.text', '**Sampson**\n\nGregory, o\' my word, we\'ll not carry coals.')
      })

      describe('JSON format', () => {
        cy.get('#export-format-json').click()
        cy.get('#export-preview', {log: false})
          .should('contain.text', '"content": "# Act I')
          .should('contain.text', '"content": "## Act 1, Prologue')
          .should('contain.text', '"content": "**Sampson**\\n\\nGregory, o\' my word, we\'ll not carry coals.')
      })
    })

    describe('Current Card & Subtree', () => {
      cy.get('#export-select-subtree').click()
      describe('Word format', () => {
        cy.get('#export-format-word').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.html', '<h1 id="act-i">Act I</h1>')
          .should('not.contain.html', '<h2 id="act-1-prologue">Act 1, Prologue</h2>')
          .should('not.contain.html', '<p><strong>Sampson</strong></p>\n<p>Gregory, o\' my word, we\'ll not carry coals.</p>')
          .should('contain.html', '<h2 id="act-1-scene-3">Act 1, Scene 3</h2>')
          .should('contain.html', '<p><strong>Lady Capulet</strong></p>\n<p>Enough of this; I pray thee, hold thy peace.</p>')
      })

      describe('Plain text', () => {
        cy.get('#export-format-text').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.text', '# Act I')
          .should('not.contain.text', '## Act 1, Prologue')
          .should('not.contain.text', '**Sampson**\n\nGregory, o\' my word, we\'ll not carry coals.')
          .should('contain.text', '## Act 1, Scene 3')
          .should('contain.text', '**Lady Capulet**\n\nEnough of this; I pray thee, hold thy peace.')
      })

      describe('JSON format', () => {
        cy.get('#export-format-json').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.text', '"content": "# Act I')
          .should('not.contain.text', '"content": "## Act 1, Prologue')
          .should('not.contain.text', '"content": "**Sampson**\\n\\nGregory, o\' my word, we\'ll not carry coals.')
          .should('contain.text', '"content": "## Act 1, Scene 3')
          .should('contain.text', '"content": "**Lady Capulet**\\n\\nEnough of this; I pray thee, hold thy peace.')
      })
    })

    describe('Current Column', () => {
      cy.get('#export-select-column').click()
      describe('Word format', () => {
        cy.get('#export-format-word').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.html', '<h1 id="act-i">Act I</h1>')
          .should('contain.html', '<h2 id="act-1-prologue">Act 1, Prologue</h2>')
          .should('not.contain.html', '<p><strong>Sampson</strong></p>\n<p>Gregory, o\' my word, we\'ll not carry coals.</p>')
          .should('contain.html', '<h2 id="act-1-scene-3">Act 1, Scene 3</h2>')
          .should('not.contain.html', '<p><strong>Lady Capulet</strong></p>\n<p>Enough of this; I pray thee, hold thy peace.</p>')
      })

      describe('Plain text', () => {
        cy.get('#export-format-text').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.text', '# Act I')
          .should('contain.text', '## Act 1, Prologue')
          .should('not.contain.text', '**Sampson**\n\nGregory, o\' my word, we\'ll not carry coals.')
          .should('contain.text', '## Act 1, Scene 3')
          .should('not.contain.text', '**Lady Capulet**\n\nEnough of this; I pray thee, hold thy peace.')
      })

      describe('JSON format', () => {
        cy.get('#export-format-json').click()
        cy.get('#export-preview', {log: false})
          .should('not.contain.text', '"content": "# Act I')
          .should('contain.text', '"content": "## Act 1, Prologue')
          .should('not.contain.text', '"content": "**Sampson**\\n\\nGregory, o\' my word, we\'ll not carry coals.')
          .should('contain.text', '"content": "## Act 1, Scene 3')
          .should('not.contain.text', '"content": "**Lady Capulet**\\n\\nEnough of this; I pray thee, hold thy peace.')
      })
    })
  })
})
