const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


describe('Random seed initialization', () => {
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

  it('Should not duplicate ids', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])

    cy.contains('Another Child card')
      .should('be.visible')
      .click()

    cy.shortcut('{ctrl}{downarrow}')
    cy.writeInCard('mod')
    cy.shortcut('{ctrl}{enter}')

    cy.contains('#save-indicator', 'Synced')

    // Switch to other tree and back
    cy.get('#documents-icon').click()
    cy.get('#sidebar-document-list-wrap').contains('Another doc').click()
    cy.url().should('contain', this.treeIds[1] )
    cy.get('#title').contains('Another doc, with title')

    cy.get('#sidebar-document-list-wrap').contains('Untitled').click()
    cy.url().should('contain', this.treeIds[0] )

    cy.contains('mod')
      .click()

    // Check for one textarea only
    cy.shortcut('{ctrl}{downarrow}')
    cy.get('textarea')
      .should('have.length', 1)
  })
})
