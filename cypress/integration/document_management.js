describe('Remote Documents', () => {
  let testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit('http://localhost:3000/new')
    cy.url().should('match', /\/[a-zA-Z0-9]{5}/)

    cy.wait(400)
    cy.writeInCard('Hello Test doc')
    cy.shortcut('{ctrl}l')
    cy.writeInCard('Child card')
    cy.shortcut('{ctrl}j')
    cy.writeInCard('Another Child card')
    cy.shortcut('{ctrl}{enter}')

    cy.contains('Synced')
    cy.visit('http://localhost:3000/logout')
    cy.clearCookie('AuthSession')

    cy.wait(400)
    cy.login(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should show the first tree in document list', () => {
    cy.visit('http://localhost:3000/')

    cy.contains('Untitled')
  })

  it('Should go to the first tree on click', () => {
    cy.contains('Untitled')
      .click()

    cy.contains('#document', 'Hello Test doc')
      .contains('#document', 'Child card')
      .contains('#document', 'Another Child card')
  })
})
