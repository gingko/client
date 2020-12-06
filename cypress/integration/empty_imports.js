const config = require("../../config.js");

describe('Imports from Empty State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout')
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit(config.TEST_SERVER)
  })

  it('Should bring up the Import Modal on clicking', () => {
    cy.get('#new-button').click()

    cy.get('#template-import-bulk')
      .click()
      .then(()=> {
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
        let loginData = {email: testEmail, password: config.LEGACY_TEST_PASSWORD, _csrf: csrf};
        cy.request({url: config.LEGACY_URL + '/auth/login', method: 'POST', body: loginData, form: true})

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

})
