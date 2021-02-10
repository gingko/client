const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");

describe('User Signup Flow', () => {
  let testEmail = 'cypress@testing.com'
  let testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Can signup using form', () => {
    // Redirects to /signup
    cy.visit(config.TEST_SERVER)
    cy.location('pathname').should('eq', '/signup')

    // Is focused on the first field
    cy.get('#signup-email').should('have.focus')

    // Displays errors on submitting empty form
    cy.get('button.cta')
      .click()

    cy.contains('Please enter an email address')
    cy.contains('Please enter a password')
    cy.contains('Please enter your password twice.')

    // Creates a new account
    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button.cta')
      .click()

    // Has an AuthSession cookie
    cy.get('button.cta').should('not.exist')
    cy.getCookie('AuthSession').should('exist')

    // Has a user database
    cy.request({url: config.TEST_SERVER + '/db/' + testUserDb, retryOnStatusCodeFailure: true})

    // Imports "Welcome Tree"
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.contains('Welcome to Gingko')

    // Logs Out Correctly
    cy.intercept('/logout').as('logoutRequest')
    cy.get('#account').click()
    cy.get('#logout-button').click()
    cy.wait('@logoutRequest')
    cy.getCookie('AuthSession').should('have.property', 'value', '')
    expect(localStorage.getItem("gingko-session-storage")).to.be.null;
    cy.location('pathname').should('eq', '/login')
    cy.get('button.cta').contains('Login')
  })

  it('Redirects to login on expired cookie', ()=>{
    cy.login(testEmail)
      .then(()=>{
        cy.clearCookie('AuthSession')
        cy.visit(config.TEST_SERVER)
        cy.location('pathname').should('eq', '/login')
      })
  })
})
