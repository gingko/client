describe('Remote Documents', () => {
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

    cy.visit('http://localhost:3000/new')
    cy.wait(400)
    cy.url().as('secondDocUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.writeInCard('Second Test doc')
    cy.shortcut('{ctrl}l')
    cy.writeInCard('# 2\nChild card')
    cy.shortcut('{ctrl}j')
    cy.writeInCard('# 3\nAnother Child card')
    cy.shortcut('{ctrl}{enter}')
    cy.contains('Synced')

    cy.get('#title h1').click()
    cy.get('#title > input')
      .type('Second doc, with title')

    cy.contains('Rename')
      .click()
    cy.wait(400)

    cy.visit('http://localhost:3000/logout')
    cy.clearCookie('AuthSession')

    cy.wait(400)
    cy.login(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should show the creates trees in document list', () => {
    cy.visit('http://localhost:3000/')

    cy.contains('#documents-block', 'Untitled')
      .contains('#documents-block', 'Second doc, with title')
  })

  it('Should go to the first tree on click', () => {
    cy.contains('Untitled')
      .click()

    cy.contains('#document', 'Hello Test doc')
      .contains('#document', 'Child card')
      .contains('#document', 'Another Child card')
  })

  it('Should show the trees in sidebar document list', () => {
    cy.get('.sidebar-button').first().click()

    cy.contains('#sidebar-menu', 'Untitled')
      .contains('#sidebar-menu', 'Second doc, with title')
  })

  it('Should navigate to tree on clicking sidebar document', function () {
    cy.get('#sidebar-menu').contains('Second').click()

    cy.url().should('equal', this.secondDocUrl)
  })
})
