const config = require("../../config.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}


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

    // Enters and exits fullscreen mode on clicking
    cy.shortcut('{shift}{enter}')
    cy.get('#app-fullscreen').should('be.visible')
    cy.get('#fullscreen-main').should('be.visible')
    cy.get('#fullscreen-exit').click()
    cy.get('#app-fullscreen').should('not.exist')
    cy.get('#fullscreen-main').should('not.exist')
    cy.get('textarea.edit').should('have.value', '# 3\nAnother Child card')
    cy.shortcut('{esc}')

    // Toggles fullscreen mode with Shift+Enter
    cy.shortcut('{shift}{enter}')
    cy.get('#app-fullscreen').should('be.visible')
    cy.get('#fullscreen-main').should('be.visible')
    cy.shortcut('{shift}{enter}')
    cy.get('#app-fullscreen').should('not.exist')
    cy.get('#fullscreen-main').should('not.exist')
    cy.get('textarea').should('have.focus')
    cy.shortcut('{shift}{enter}')
    cy.get('#app-fullscreen').should('be.visible')
    cy.get('#fullscreen-main').should('be.visible')

    // Textareas in fullscreen mode are private
    cy.get('textarea').each((el) => {
      expect(el).to.have.attr("data-private","lipsum")
    })

    // Test typing
    cy.focused().type(' test')
    cy.get('#fullscreen-buttons #save-indicator').contains('Unsaved Changes...')
    cy.focused().should('have.value', '# 3\nAnother Child card test')

    // Test change card focus
    cy.get('textarea').first().focus()
      .type('{enter}abc')
    cy.shortcut('{ctrl}{enter}')
    cy.getCard(2,1,1).should('contain', 'cardabc')
    cy.getCard(2,1,2).should('contain', 'card test')

    // Field preserved when exiting fullscreen
    cy.shortcut('{shift}{enter}')
    cy.focused().type('lmn')
    cy.get('#fullscreen-exit').click()
    cy.focused()
      .should('have.value','# 2\nChild card\nabclmn')

    // Save and exit edit mode on Ctrl+Enter
    cy.shortcut('{shift}{enter}')
    cy.focused().type(' line')
    cy.shortcut('{ctrl}{enter}')
    cy.get('#app-fullscreen').should('not.exist')
    cy.get('#fullscreen-main').should('not.exist')
    // Wait for synced before proceeding
    cy.contains('Synced', {timeout: 20000})
    cy.get('textarea').should('not.exist')
    cy.getCard(2,1,1).should('contain', 'cardabclmn line')

    // Save and don't exit edit mode on Ctrl+S
    cy.shortcut('{downarrow}{shift}{enter}')
    cy.get('#fullscreen-main').should('be.visible')
    cy.focused().type('xyz')
    cy.get('#fullscreen-buttons #save-indicator').should('be.visible')
    cy.shortcut('{ctrl}s')
    cy.get('#app-fullscreen')
    cy.get('#fullscreen-main')
    cy.get('#fullscreen-buttons #save-indicator').should('contain', 'Synced')

    cy.shortcut('{esc}')

    // Make sure that changes in non-fullscreen are "transferred" to fullscreen
    // when toggled from Edit mode.
    cy.shortcut('{enter}')
    cy.get('textarea').type('edit')
    cy.shortcut('{shift}{enter}')
    cy.get('.active-fullscreen textarea').should('have.value', '# 3\nAnother Child card testxyzedit')
    cy.shortcut('{ctrl}{enter}')
    cy.getCard(2,1,2).should('contain.html', '<p>Another Child card testxyzedit</p>')

    // Wait for synced before proceeding
    cy.contains('Saved Offline', {timeout: 30000})
    cy.contains('Synced', {timeout: 30000})
  })

  it('Saved fullscreen changes correctly', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[1])

    cy.getCard(1,1,1).should('contain.html', '<p>Another Test doc</p>')
    cy.getCard(2,1,1).should('contain.html', '<p>Child card<br>abclmn line</p>')
    cy.getCard(2,1,2).should('contain.html', '<p>Another Child card testxyzedit</p>')
  })
})
