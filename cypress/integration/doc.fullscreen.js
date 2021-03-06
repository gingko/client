const config = require("../../config.js");

describe('Fullscreen Editing', () => {
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

  it('Can perform basic actions on New tree', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[1])

    cy.contains('Another Child card')
      .should('be.visible')
      .click()

    // Enters and exits fullscreen mode
    cy.shortcut('{shift}{enter}')
    cy.get('#app-fullscreen').should('be.visible')
    cy.get('#fullscreen-main').should('be.visible')
    cy.get('#fullscreen-exit').click()
    cy.get('#app-fullscreen').should('not.exist')
    cy.get('#fullscreen-main').should('not.exist')
    cy.get('textarea.edit').should('have.value', '# 3\nAnother Child card')
  })
})
