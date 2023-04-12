const config = require("../../config.js");

describe('Keyboard Shortcuts', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
      cy.fixture('twoTrees.ids.json').as('treeIds')
    })
  })


  it('Has working shortcuts', function () {
    cy.get('@treeIds').then((treeIds) => {
      // TODO : This is a hack, due to issue with going directly
      // to a card-based document before the tree list is loaded.
      cy.visit(config.TEST_SERVER)
      cy.wait(1000)


      cy.visit(config.TEST_SERVER + '/' + treeIds[0])

      cy.get('#app-root').should('be.visible')
      cy.get('.spinner').should('not.exist')

      cy.url().should('contain', treeIds[0] )
    })


    describe('In document mode', () => {
      cy.shortcut('{?}')
      cy.get('.modal.help-modal')
        .should('be.visible')

      cy.shortcut('{?}')
      cy.get('.modal.help-modal')
        .should('not.exist')

      cy.shortcut('w')
      cy.get('.modal-header h2')
        .contains('Word & Character Counts')

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

