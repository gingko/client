const config = require("../../config.js");
const { tr } = require("../../src/shared/translation.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Document UI', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup(testEmail)
    cy.visit(config.TEST_SERVER + '/new')
    cy.wait(400)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Opens Help menu dropdown on clicking icon', () => {
    let emailText = tr["emailSupport"]["en"];

    cy.get('#app-root')
      .should('not.contain', emailText)

    cy.get('#help-icon' )
      .click()

    cy.get('#help-dropdown')
      .contains(emailText)
  })

  it('Triggers mailto action on click', () => {
    cy.get('#email-support')
      .click()

    cy.window().then((win) => {
      expect(win.elmMessages.slice(-1)[0]).to.eq("TriggerMailto")
    })
  })

  it('Toggles "Open" modal on "Ctrl+O"', () => {
    cy.get('#switcher-modal').should('not.exist')

    cy.shortcut('{ctrl}o')
    cy.get('#switcher-modal').should('exist')

    cy.shortcut('{ctrl}o')
    cy.get('#switcher-modal').should('not.exist')
  })

  it('It autofocuses on "Open" modal input', () => {
    cy.shortcut('{ctrl}o')

    cy.get('#switcher-modal input').should('have.focus')
  })

  it('Closes "Open" modal on "Esc"', () => {
    cy.shortcut('{esc}')
    cy.get('#switcher-modal').should('not.exist')
  })

  it('Toggles shortcut tray on clicking right-sidebar', () => {
    cy.contains('Keyboard Shortcuts')

    cy.get('#shortcuts-tray').click({position: "top"})

    cy.get('#app-root').should('not.contain', 'Keyboard Shortcuts')
  })

  it('Shows different shortcuts based on mode', () => {
    cy.get('#shortcuts-tray').click({position: "top"})
    cy.contains('(Edit Mode)')

    cy.writeInCard('This is a test')

    cy.shortcut('{ctrl}{enter}')

    cy.get('#app-root').should('not.contain', '(Edit Mode)')
  })

  it('Opens Markdown Format guide in external window', () => {
    cy.shortcut('{enter}')
    cy.get('#shortcuts a').should('have.attr', 'target', '_blank')
  })
})