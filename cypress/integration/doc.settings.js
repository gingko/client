const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}


describe('Loading indicators', () => {
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

  it('Can change the document language', () => {
    cy.visit(config.TEST_SERVER)
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.get('.spinner').should('not.exist')
    cy.contains('Synced')

    cy.get('#account-icon').click()
    cy.get('#language-option').click()
    cy.get('#lang-es').click()

    cy.contains('Sincronizado')
  })

  it('Persists the language setting on reload', () => {
    cy.visit(config.TEST_SERVER)
    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)
    cy.contains('Sincronizado')
  })

  it('Saves last active position', () => {
    cy.get('#app-root')
      .contains('Another Test doc')

    cy.getCard(1,1,1)
      .should('have.class', 'active')

    cy.getGroup(2,1)
      .should('have.class', 'active-descendant')

    // Select first child
    cy.shortcut('{rightArrow}')

    cy.getCard(2,1,1)
      .should('have.class', 'active')

    cy.wait(400)

    // Reload
    cy.reload()

    cy.wait(400)

    // First child should still be selected
    cy.getCard(2,1,1)
      .should('have.class', 'active')

    cy.get('#documents-icon')
      .click()

    cy.get('#sidebar-document-list-wrap').contains('Untitled').click()

    cy.getCard(1,1,1)
      .should('have.class', 'active')

    cy.getGroup(2,1)
      .should('have.class', 'active-descendant')

    // Select second child
    cy.shortcut('{rightArrow}{downArrow}')

    cy.getCard(2,1,2)
      .should('have.class', 'active')

    cy.wait(4000)

    // Go back to first document
    cy.get('#sidebar-document-list-wrap').contains('Another doc, with title').click()

    // First child should still be selected
    cy.getCard(2,1,1)
      .should('have.class', 'active')

    // Go back to second document
    cy.get('#sidebar-document-list-wrap').contains('Untitled').click()

    // Second child should still be selected
    cy.getCard(2,1,2)
      .should('have.class', 'active')
  })
})
