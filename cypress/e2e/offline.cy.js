const config = require("../../config.js");


describe.skip('Offline Tests', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(()=>{
      cy.signup(testEmail)
    })
  })

  it('Can save offline changes', () => {
    cy.visit(config.TEST_SERVER + '/new')
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.get('#app-root').should('be.visible')
    cy.get('.spinner').should('not.exist')

    cy.writeInCard('Hello Test doc')
    cy.shortcut('{ctrl}l')
    cy.writeInCard('Child card')
    cy.shortcut('{ctrl}j')
    cy.writeInCard('Another Child card')
    cy.shortcut('{ctrl}{enter}')
    cy.contains('Synced')

    /*
    cy.intercept('/db*', {forceNetworkError: true}).as('offline')
    cy.wait('@offline')

     */

    cy.shortcut('{enter}')
    cy.writeInCard('\nSome offline changes')
    cy.shortcut('{ctrl}{enter}')
    cy.contains('Saved Offline')
  })
})
