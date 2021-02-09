const config = require("../../config.js");

describe('JSON Imports from Empty State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.request(config.LEGACY_URL + '/logout').then(()=>{
      cy.deleteUser(testEmail).then(()=>{
        cy.signup(testEmail).then(()=>{
          cy.visit(config.TEST_SERVER)
        })
      })
    })
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should bring up the Import Modal on clicking', () => {
    cy.get('#new-button').click()

    cy.get('#template-import')
      .click()
  })
})
