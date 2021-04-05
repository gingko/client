const config = require("../../config.js");

/*

NOTE: This only works in Headless mode, for some reason.
In browser mode, doesn't login to legacy gingko correctly.

 */
describe('Legacy Imports from Startup State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout')
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Guides user to import legacy docs', function () {
    cy.visit(config.TEST_SERVER)
    cy.url().should('contain', this.treeIds[1] )
    cy.get('.spinner').should('not.exist')

    // Should bring up the Import Modal on clicking
    cy.get('#new-icon').click()

    cy.get('#template-import-bulk')
      .click()
      .then(() => {
        cy.contains('Import From Gingko v1')
      })

    // If not logged in at legacy, asks user to
    cy.contains('you are not logged in')

    // If logged in at legacy, show download link
    cy.intercept(config.LEGACY_URL + '/loggedin',
      "<html>\n" +
      "<head></head>\n" +
      "<body>\n" +
      "<script>\n" +
      "window.parent.postMessage({loggedin: true}, \"*\");\n" +
      "</script>\n" +
      "</body>\n" +
      "</html>")
    cy.get('#retry-button')
      .click()

    cy.contains('Download Full Backup')

    // Shows tree list from the dropped file
    cy.get('.file-drop-zone')
      .attachFile('bulk-import-test-larger.txt', { subjectType: 'drag-n-drop' })

    cy.get('#import-selection-list')
      .should('contain', 'Screenplay')
      .and('contain', 'Timeline')
      .and('contain', 'Example Tree')

    // Adds selected trees to document list
    cy.viewport('samsung-s10', 'landscape')

    cy.get('#import-select-all')
      .click()

    cy.get('.modal-guts button')
      .click()

    cy.contains('Importing selected 10 trees...')

    // Closed the Import Modal on success
    cy.get('#app-root').should('not.contain', 'Import From Gingko v1')

    cy.get('#documents-icon').click()
    cy.get('#sidebar-document-list > .sidebar-document-item')
      .should('have.length', 12)

    // Correctly imported the data
    cy.get('#sidebar-document-list')
      .contains('Screenplay')
      .click()

    cy.url().as('importedScreenplayUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('tips to improve your logline')

    // Does not contain data from other trees
    cy.get('#document')
      .should('not.contain', 'October')
      .and('not.contain', 'example gingko tree')
  })
})