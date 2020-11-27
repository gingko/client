const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");

describe('User Signup Flow', () => {
  let testEmail = 'cypress@testing.com'
  let testUserDb = helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Redirects to /signup', () => {
    cy.visit(config.TEST_SERVER)
    cy.location('pathname').should('eq', '/signup')
  })

  it('Is focused on the first field', () => {
    cy.get('#signup-email').should('have.focus')
  })

  it('Displays errors on submitting empty form', () => {
    cy.get('button.cta')
      .click()

    cy.contains('Please enter an email address')
    cy.contains('Please enter a password')
    cy.contains('Please enter your password twice.')
  })

  it('Creates a new account', () => {
    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button.cta')
      .click()

  })

  it('Has an AuthSession cookie', () => {
    cy.wait(400)
    cy.getCookie('AuthSession').should('exist')
  })

  it('Has a user database', () => {
    cy.request({url: config.TEST_SERVER + '/db/userdb-' + testUserDb, retryOnStatusCodeFailure: true})
  })

  it('Imports "Welcome Tree"', ()=> {
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.contains('Welcome to Gingko')
  })

  it('Logs Out Correctly', () =>{
    cy.get('#account').click()
    cy.get('#logout-button').click()
    cy.wait(400)
    cy.getCookie('AuthSession').should('have.property', 'value', '')
    cy.location('pathname').should('eq', '/login')
    cy.get('button.cta').contains('Login')
  })
})
