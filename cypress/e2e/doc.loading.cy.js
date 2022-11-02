const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


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

  it('Should not show "Empty" message', () => {
    cy.visit(config.TEST_SERVER)

    cy.url().should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.window().then((win) => {
      expect(win.elmMessages.map(m => m.tag))
        .to.not.include('EmptyMessageShown');
    })
  })
})
