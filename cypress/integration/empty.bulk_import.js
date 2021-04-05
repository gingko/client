const config = require("../../config.js");

describe('Legacy Imports from Empty State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout').then(()=>{
      cy.deleteUser(testEmail).then(() => {
        cy.signup_with(testEmail, 'empty').then(()=>{
          cy.visit(config.TEST_SERVER)
        })
      })
    })
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should allow import of legacy docs from empty state', () => {
    cy.get('#new-icon').click()

    cy.get('#template-import-bulk')
      .click()
      .then(()=> {
        cy.contains('Import From Gingko v1')
      })

    // If not logged in at legacy, asks user to
    cy.contains('you are not logged in')

    // If logged in at legacy, show download link
    let csrf = ''
    cy.request(config.LEGACY_URL + '/login')
      .then((resp) => {
        csrf = /<meta.*"csrf" content="(.*)">/.exec(resp.body)[1];
        let loginData = {email: testEmail, password: config.LEGACY_TEST_PASSWORD, _csrf: csrf};
        cy.request({url: config.LEGACY_URL + '/auth/login', method: 'POST', body: loginData, form: true})

        cy.get('#retry-button')
          .click()

        cy.contains('Download Full Backup')
      })

    // Shows tree list from the dropped file
    cy.get('.file-drop-zone')
      .attachFile('bulk-import-test.txt', { subjectType: 'drag-n-drop' })

    cy.get('#import-selection-list')
      .should('contain', 'Screenplay')
      .and('contain', 'Timeline')
      .and('contain', 'Example Tree')

    // Adds selected trees to document list
    cy.get('#import-selection-list input')
      .click({multiple : true})

    cy.get('.modal-guts button')
      .click()

    cy.wait(400)

    cy.get('#file-button')
      .click()

    cy.get('.sidebar-document-list > .sidebar-document-item')
      .should('have.length', 3)

    // Should navigate to last modified document
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('tips to improve your logline')

    // Closed the Import Modal on success
    cy.get('#app-root').should('not.contain', 'Import From Gingko v1')
  })
})
