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

    // Imports a text file successfully
    cy.get('#template-import-text')
      .click()

    cy.get('#import-text-file-input')
      .click()

    cy.contains('This is a test file')
  })
})