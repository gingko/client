const config = require("../../config.js");

describe('Keyboard Shortcuts', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Has working shortcuts', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.url().should('contain', this.treeIds[0] )

    cy.get('#app-root')

    describe('In document mode', () => {
      cy.shortcut('{?}')
      cy.get('.modal.help-modal')
        .should('be.visible')

      cy.shortcut('{?}')
      cy.get('.modal.help-modal')
        .should('not.exist')

      cy.shortcut('w')
      cy.get('.modal-header h2')
        .contains('Word Counts')

      cy.shortcut('w')
      cy.get('.modal-header h2')
        .should('not.exist')
    })

    describe('Does not trigger shortcuts from edit mode', () => {
      cy.shortcut('{enter}')
      cy.writeInCard('?')

      cy.get('.modal.help-modal')
        .should('not.exist')

      cy.writeInCard('w')
      cy.get('.modal-header h2')
        .should('not.exist')
    })

    describe('Does not trigger from Quick Switcher', () => {
      cy.shortcut('{ctrl}{enter}')
      cy.shortcut('{rightArrow}')
      cy.getCard(2,1,1)
        .should('have.class', 'active')

      cy.shortcut('{ctrl}o')

      cy.get('#switcher-modal')
        .should('be.visible')

      cy.shortcut('j')

      cy.getCard(2,1,1)
        .should('have.class', 'active')

      cy.shortcut('k')

      cy.getCard(2,1,1)
        .should('have.class', 'active')

      cy.shortcut('h')

      cy.getCard(2,1,1)
        .should('have.class', 'active')

      cy.shortcut('{esc}')

      cy.get('#switcher-modal')
        .should('not.exist')

    })
  })
})

