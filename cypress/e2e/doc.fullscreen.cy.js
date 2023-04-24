const config = require("../../config.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}


describe('Fullscreen Editing', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
      cy.fixture('twoTrees.ids.json').as('treeIds')
    })
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
    cy.get('textarea').should('have.focus')

    // Textareas in fullscreen mode are private
    cy.get('textarea').each((el) => {
      expect(el).to.have.attr("data-private","lipsum")
    })

    // Test typing
    cy.focused().type(' test', {delay: 50})
    cy.get('#fullscreen-buttons #save-indicator').contains('Synced')
    cy.focused().should('have.value', '# 3\nAnother Child card test')

    // Test change card focus
    cy.get('textarea').first().click()
      .type('{enter}abc', {delay: 50})
    cy.wait(100)
    cy.get('#fullscreen-buttons #save-indicator').contains('Synced')
    cy.shortcut('{esc}')
    cy.get('#app-fullscreen').should('not.exist')
    cy.get('#fullscreen-main').should('not.exist')
    cy.get('textarea').should('not.exist')
    cy.getCard(2,1,1).should('contain', 'cardabc')
    cy.getCard(2,1,2).should('contain', 'card test')

    // Make sure cardabc doesn't have newline at end
    cy.getCard(2,1,1).click()
    cy.get('.edit').click()
    cy.get('textarea').should('have.value', '# 2\nChild card\nabc')
    cy.shortcut('{esc}')

    // Field preserved when exiting fullscreen
    cy.shortcut('{shift}{enter}')
    cy.focused().type('lmn', {delay: 50})
    cy.get('#fullscreen-exit').click()
    cy.focused()
      .should('have.value','# 2\nChild card\nabclmn')

    // Save and exit edit mode on Ctrl+Enter
    cy.get('.fullscreen-card-btn').click()
    cy.focused().type(' line', {delay: 50})
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
    cy.focused().type('xyz', {delay: 50})
    cy.get('#fullscreen-buttons #save-indicator').should('be.visible')
    cy.shortcut('{ctrl}s')
    cy.get('#app-fullscreen')
    cy.get('#fullscreen-main')
    cy.get('#fullscreen-buttons #save-indicator').should('contain', 'Synced')

    cy.shortcut('{esc}')

    // Saved fullscreen changes correctly
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[1])

    cy.getCard(1,1,1).should('contain.html', '<p>Another Test doc</p>')
    cy.getCard(2,1,1).should('contain.html', '<p>Child card<br>abclmn line</p>')
    cy.getCard(2,1,2).should('contain.html', '<p>Another Child card testxyz</p>')
  })
})
