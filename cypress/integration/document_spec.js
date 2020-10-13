const helpers = require("../../src/shared/doc-helpers.js");

describe('Document Editing', () => {
  let testEmail = 'test'+Date.now()+'@testing.com'
  let testUserDb = helpers.toHex(testEmail);

  before(() => {
    cy.visit('http://localhost:3000/')

    cy.get('#signup-email')
      .type(testEmail)

    cy.get('#signup-password')
      .type('testing')

    cy.get('#signup-password-confirm')
      .type('testing')

    cy.get('button')
      .contains('Signup')
      .click()
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Creates a new blank tree', () => {
    cy.contains('Blank Tree')
      .click()

    cy.url().should('match', /\/[a-zA-Z0-9]{5}/)

    cy.contains('Untitled')
    cy.contains('New Document...')
  })

  it('Can edit and save card', () => {
    cy.get('textarea').should('have.focus')
      .type('Hello World :)')

    cy.get('body').type('{ctrl}{enter}')

    cy.get('#card-1 .view').contains('Hello World :)')
  })

  it('Is marked as "Synced"', () => {
    cy.contains('Synced')
  })

  it('Can rename the document', () => {
    cy.get('#title h1').click()
    cy.get('#title > input')
      .should('have.focus')
      .type('A new doc title here')

    cy.contains('Rename')
      .click()

    cy.get('#title h1').contains('A new doc title here')
  })
})