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

  it('Has a user database', () => {
    cy.wait(400)
    cy.request({url: config.TEST_SERVER + '/db/userdb-' + testUserDb, retryOnStatusCodeFailure: true})
  })

  it('Has an AuthSession cookie', () => {
    cy.getCookie('AuthSession').should('exist')
  })

  it('Redirects to Home page', ()=> {
    cy.location('pathname').should('eq', '/')
    cy.contains('Blank Tree')
  })

  it('Logs Out Correctly', () =>{
    cy.get('button.logout').click()
    cy.wait(400)
    cy.getCookie('AuthSession').should('have.property', 'value', '')
    cy.location('pathname').should('eq', '/login')
  })
})
