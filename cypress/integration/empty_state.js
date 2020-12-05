const config = require("../../config.js");


describe('Managing Documents', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit(config.TEST_SERVER)
  })

  it('Should show Empty page', ()=> {
    cy.url().should('eq', config.TEST_SERVER + '/')
    cy.contains("You don't have any documents. Create one here:")
  })

  it('Should show Template Selector on clicking "New"', () => {
    cy.get('#new-button').click()

    cy.get('#templates-block')
  })
})
