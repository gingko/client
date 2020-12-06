const config = require("../../config.js");

describe('Imports from Empty State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout')
    cy.deleteUser(testEmail)
    cy.visit(config.TEST_SERVER)

    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button.cta')
      .click()
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should bring up the Import Modal on clicking', () => {
    cy.get('#file-button').click()

    cy.get('#new-button').click()

    cy.get('#template-import-bulk')
      .click()
      .then(() => {
        cy.contains('Import From Gingko v1')
      })
  })

  it('If not logged in at legacy, asks user to', () => {
    cy.contains('you are not logged in')
  })

  it('If logged in at legacy, show download link', () => {
    let csrf = ''
    cy.request(config.LEGACY_URL + '/login')
      .then((resp) => {
        csrf = /<meta.*"csrf" content="(.*)">/.exec(resp.body)[1];
        let loginData = { email: testEmail, password: config.LEGACY_TEST_PASSWORD, _csrf: csrf };
        cy.request({ url: config.LEGACY_URL + '/auth/login', method: 'POST', body: loginData, form: true })

        cy.get('.modal-guts button')
          .click()

        cy.contains('Download Full Backup')
      })
  })

  it('Shows tree list from the dropped file', () => {
    cy.get('.file-drop-zone')
      .attachFile('bulk-import-test.txt', { subjectType: 'drag-n-drop' })

    cy.get('#import-selection-list')
      .should('contain', 'Screenplay')
      .and('contain', 'Timeline')
      .and('contain', 'Example Tree')
  })

  it('Adds selected trees to document list', () => {
    cy.get('#import-selection-list input')
      .click({ multiple: true })

    cy.get('.modal-guts button')
      .click()

    cy.get('.sidebar-document-list > .sidebar-document-item')
      .should('have.length', 4)
  })

  it('Closed the Import Modal on success', () => {
    cy.get('#app-root').should('not.contain', 'Import From Gingko v1')
  })

  it('Correctly imported the data', () => {
    cy.get('.sidebar-document-list')
      .contains('Screenplay')
      .click()

    cy.url().as('importedScreenplayUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('tips to improve your logline')
  })

  it('Does not contain data from other trees', () => {
    cy.get('#document')
      .should('not.contain', 'October')
      .and('not.contain', 'example gingko tree')
  })
})