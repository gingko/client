const config = require("../../config.js");

describe('JSON Imports from Startup State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should bring up the Import Modal on clicking', function () {
    cy.visit(config.TEST_SERVER)
    cy.url().should('contain', this.treeIds[1] )
    cy.get('.spinner').should('not.exist')
    cy.contains('Another Test doc')

    cy.get('#new-icon').click()

    //Brings up Import Text Files dialog
    cy.get('#template-import-text')
      .click()

    cy.get('#import-text-modal')
      .should('be.visible')
    cy.get('.modal-header')
      .contains('Import Text Files')

    // Goes back to template selector on close button
    cy.get('.close-button')
      .click()

    cy.get('#template-import-text')
      .should('be.visible')

    cy.get('#template-import-text')
      .click()

    cy.get('#import-text-modal')
      .should('be.visible')

    cy.get('#no-splitting')
      .should('be.checked')


    cy.get('#split-by-paragraph')
      .should('not.be.checked')
      .click()
      .should('be.checked')

    cy.get('#no-splitting')
      .should('not.be.checked')

    cy.get('#split-by-paragraph')
      .should('be.visible')

    describe('Imports a text file successfully with split-by-paragraph', () =>{
      cy.get('#import-text-file-input')
        .click()

      cy.get('#import-text-perform')
        .click()

      // Importing one file should title the
      // document based on filename
      cy.get('#title-rename')
        .should('have.value', 'foo')

      cy.getCard(1,1,1)
        .should('contain','This is a test file.')
        .should('not.contain', 'With a paragraph break.')

      cy.getCard(1,1,4)
        .should('contain', 'And a split break.')
    })

    describe('Imports a text file successfully with split-by-separator', () => {
      cy.get('#new-icon').click()
      cy.get('#template-import-text').click()
      cy.get('#import-text-file-input').click()
      cy.get('#split-by-separator').click()
      cy.get('#import-text-perform').click()

      cy.get('#title-rename')
        .should('have.value', 'foo2')

      cy.getCard(1,1,1)
        .should('contain', 'Test file two.')
        .should('contain', 'With a paragraph break.')
        .should('not.contain', 'And a split break.')

      cy.getCard(1,1,2)
        .contains('And a split break.')
    })

    describe('Imports multiple files, one per card', ()=>{
      cy.get('#new-icon').click()
      cy.get('#template-import-text').click()
      cy.get('#import-text-file-input').click()
      cy.get('#import-text-perform').click()

      cy.get('#title-rename')
        .should('have.value', 'Untitled')

      cy.getCard(1,1,1)
        .should('contain', 'bar1')

      cy.getCard(1,1,2)
        .should('contain', 'bar2')
    })
  })
})