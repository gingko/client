describe('Document Editing', () => {
  let testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Creates a new blank tree', () => {
    cy.visit('http://localhost:3000/')
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