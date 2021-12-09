const config = require("../../config.js");


describe('Empty State', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(()=>{
      cy.signup(testEmail).then(()=>{
        cy.visit(config.TEST_SERVER)
      })
    })
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Shows Empty page when appropriate', ()=> {
    cy.url().should('eq', config.TEST_SERVER + '/')
    cy.contains("You don't have any documents")

    describe('Goes to newly created tree', () => {
      cy.get('#new-button').click()

      cy.get('#templates-block')
        .get('#template-new')
        .click()

      cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    })

    describe('Goes back to Empty page on deletion', () => {
      cy.get('#documents-icon').click()

      cy.get('#sidebar-document-list-wrap .sidebar-document-item')
        .first()
        .rightclick()

      cy.contains('Delete Tree')
        .click()

      cy.url().should('equal', config.TEST_SERVER + '/')

      cy.get('#no-documents')

      cy.contains("You don't have any documents")
    })
  })
})
