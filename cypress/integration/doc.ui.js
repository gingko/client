const config = require("../../config.js");
const { tr } = require("../../src/shared/translation.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Document UI', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup(testEmail).then(()=>{
        cy.visit(config.TEST_SERVER + '/new')
      })
    })
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Has working header menus and shortcut help', () => {
    let emailText = tr["emailSupport"]["en"];

    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.get('#app-root')
      .should('not.contain', emailText)

    // Wait for trial notice, to prevent DOM shift
    // that causes cypress to miss the #help-icon click
    cy.contains('14 days left')

    cy.get('#help-icon' )
      .click()

    cy.get('#help-dropdown')
      .should('contain', emailText)
      .should('contain', 'FAQ')

    // Triggers mailto action on click
    cy.get('#email-support')
      .click()

    cy.get('#contact-form')
      .should('be.visible')

    cy.get('#contact-from-email')
      .should('have.value', testEmail)

    cy.get('#contact-to-email')
      .should('have.value', config.SUPPORT_EMAIL)

    cy.get('#contact-subject')
      .should('have.value', 'Please help!')

    cy.get('#contact-body')
      .should('have.focus')
      .type('Help me!')

    cy.get('#contact-send')
      .click()

    cy.get('#contact-form')
      .should('not.exist')

    // Toggles the sidebar on clicking brand icon
    cy.get('#sidebar-menu').should('not.exist')
    cy.get('#brand').click()
    cy.get('#sidebar-menu').should('be.visible')
    cy.get('#brand').click()
    cy.get('#sidebar-menu').should('not.exist')

    // Toggles shortcut tray on clicking right-sidebar
    cy.contains('Keyboard Shortcuts')

    cy.get('#shortcuts-tray').click({position: "top"})

    cy.get('#app-root').should('not.contain', 'Keyboard Shortcuts')

    // Shows different shortcuts based on mode
    cy.get('#shortcuts-tray').click({position: "top"})
    cy.contains('(Edit Mode)')

    cy.writeInCard('This is a test')

    cy.shortcut('{ctrl}{enter}')

    cy.get('#app-root').should('not.contain', '(Edit Mode)')

    // Opens Markdown Format guide in external window
    cy.shortcut('{enter}')
    cy.get('#shortcuts a').should('have.attr', 'target', '_blank')
    cy.shortcut('{esc}')

    // Has working Word Count modal
    cy.get('#wordcount')
      .click()

    cy.get('.modal-header h2').contains('Word Counts')

    cy.contains('Total : 4 words')

    cy.shortcut('{esc}')

  })

  it('Has working mobile buttons', () => {
    // Displays Mobile buttons on smaller screens
    cy.get('#mobile-buttons')
      .should('not.be.visible')

    cy.viewport(360, 640)
    cy.get('#mobile-buttons')
      .should('be.visible')

    cy.get('#mbtn-edit')
      .click()

    cy.get('textarea')
      .should('exist')
      .should('have.focus')

    cy.writeInCard('{enter}here')

    cy.get('#mbtn-save')
      .click()

    cy.get('.view').contains('here')

    // Test "add child"
    cy.get('#mbtn-add-right').click()
    cy.writeInCard('axc')
    cy.get('#mbtn-save').click()
    cy.getColumn(2).contains('axc')

    // Test "add below"
    cy.get('#mbtn-add-down').click()
    cy.writeInCard('sdf')
    cy.get('#mbtn-save').click()
    cy.getCard(2,1, 2).contains('sdf')

    // Test "add above"
    cy.get('#mbtn-add-up').click()
    cy.writeInCard('lak')
    cy.get('#mbtn-save').click()
    cy.getCard(2,1, 2).contains('lak')

    // Test "nav up"
    cy.get('#mbtn-nav-up').click()
    cy.get('.card.active').should('contain', 'axc')

    // Test "nav down"
    cy.get('#mbtn-nav-down').click()
    cy.get('#mbtn-nav-down').click()
    cy.get('.card.active').should('contain', 'sdf')

    // Test "nav left"
    cy.get('#mbtn-nav-left').click()
    cy.get('.card.active').should('contain', 'This is a test')

    // Test "nav right"
    cy.get('#mbtn-nav-right').click()
    cy.get('.card.active').should('contain', 'sdf')

    // Test "cancel" button
    cy.get('#mbtn-edit').click()
    cy.get('#mbtn-cancel').click()
    cy.get('textarea').should('not.exist')
  })
})