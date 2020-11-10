const config = require("../../config.js");


describe.skip('Offline Tests', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit('http://localhost:3000/new')
    cy.wait(400)
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.writeInCard('Hello Test doc')
    cy.shortcut('{ctrl}l')
    cy.writeInCard('Child card')
    cy.shortcut('{ctrl}j')
    cy.writeInCard('Another Child card')
    cy.shortcut('{ctrl}{enter}')
    cy.contains('Synced')
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Can save offline changes', () => {
    cy.route2('*', (req) => {req.destroy()})
    cy.shortcut('{enter}')
    cy.writeInCard('\nSome offline changes')
    cy.shortcut('{ctrl}{enter}')
    cy.contains('Saved Offline')
  })
})
