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

    cy.window().then((win) => {
      expect(win.elmMessages.slice(-1)[0].tag).to.eq("TriggerMailto")
    })

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
  })
})