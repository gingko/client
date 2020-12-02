const config = require("../../config.js");
const { tr } = require("../../src/shared/translation.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Document Editing', () => {
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
})