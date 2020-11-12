const config = require("../../config.js");


describe('User Signup Flow', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout')
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Toggles the Import Modal', () => {
    cy.visit(config.TEST_SERVER)
    cy.get('.import-bulk')
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

    cy.contains('Untitled tree')
  })

  it('Adds selected trees to document list', () => {
    cy.get('input')
      .click()

    cy.get('.modal-guts button')
      .click()

    cy.get('.document-list > .document-item')
      .should('have.length', 1)

    cy.get('.doc-title')
      .contains('Untitled tree')
  })

  it('Correctly imported the data', () => {
    cy.get('.doc-title')
      .click()

    cy.url().as('importedDocUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('Test tree "root"')

    cy.get(':nth-child(3) > .group')
      .then((el) => {
        expect(el.children('.card').toArray().map(c => c.innerText))
          .to.equal(["First Child", "Second Child", "Third Child"])
      })
  })
})
