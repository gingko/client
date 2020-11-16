const config = require("../../config.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Document Editing', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit(config.TEST_SERVER)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Creates a new blank tree', () => {
    cy.contains('Blank Tree')
      .click()

    cy.url().as('testTreeUrl').should('match', /\/[a-zA-Z0-9]{5}/)

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

  it('Create a new child on clicking right + button', () => {
    cy.get('#card-1').trigger('mouseover')

    cy.get('.ins-right').click()

    cy.get('textarea').should('have.focus')
      .type('A child')
  })

  it('Saves the card by clicking the checkmark', () => {
    cy.get('.card-btn')
      .click()

    cy.get('div.card.active')
      .contains('A child')
  })

  it('Create and saves a card below using shortcuts', () => {
    cy.get('body').type('{ctrl}j')

    cy.get('textarea').should('have.focus')
      .type('Another one below')

    cy.get('body').type('{ctrl}{enter}')

    cy.get('div.card.active')
      .contains('Another one below')

    cy.contains('Synced')
  })

  it('Can rename the document', function () {
    cy.get('#title h1').click()
    cy.get('#title input')
      .should('have.focus')
      .type('A new doc title here')

    cy.contains('Rename')
      .click()

    cy.get('#title h1').contains('A new doc title here')
  })

  it('Has saved the content', function () {
    cy.wait(400)
    cy.visit(this.testTreeUrl)
    cy.get(':nth-child(3) > .group:nth-child(2)')
      .contains('Another one below')
  })

  it('Has saved the activation state', () => {
    cy.wait(400)
    cy.get('#card-1')
      .should('have.class', 'ancestor')

    cy.get(':nth-child(3) > .group')
      .should('have.class', 'has-active')
      .should('not.have.class', 'active-descendant')

    cy.get(':nth-child(3) > .group :nth-child(2)')
      .should('have.class', 'active')
  })

  it('Filters cards on search', () => {
    cy.get('#search-field')
      .click()
      .type('another')

    cy.get('#app-root')
      .should('not.contain', 'Hello World :)')
  })

  it('Removes filters on clearing search', () => {
    cy.get('#search-input')
      .clear()

    cy.contains('Hello World :)')
  })
})