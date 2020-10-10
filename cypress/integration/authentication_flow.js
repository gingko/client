describe('User Signup Flow', () => {
  let testEmail;

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Redirects to /signup', () => {
    cy.visit('http://localhost:3000/')
    cy.location('pathname').should('eq', '/signup')

    Cypress.Cookies.debug(true)
  })

  it('Displays errors on submitting empty form', () => {
    cy.get('button')
      .contains('Signup')
      .click()

    cy.contains('Please enter an email address')
    cy.contains('Please enter a password')
    cy.contains('Please enter your password twice.')
  })

  it('Creates a new account', () => {
    testEmail = 'test'+Date.now()+'@testing.com'
    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button')
      .contains('Signup')
      .click()

    cy.location('pathname').should('eq', '/')
    cy.contains('Blank Tree')
  })

  it('Creates a new blank tree', () => {
    cy.contains('Blank Tree')
      .click()

    cy.url().should('match', /\/[a-zA-Z0-9]{5}/)

    cy.contains('Untitled')
    cy.contains('New Document...')
  })

  it('Is focused on editing card', () => {
    cy.get('textarea').should('have.focus')
      .type('Hello World :)')

    cy.get('body').type('{ctrl}{enter}')

    cy.get('#card-1 .view').contains('Hello World :)')
  })
})