const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


describe('Loading indicators', () => {
  const testEmail = 'cypress@testing.com'
  const testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup_blank(testEmail)

    cy.task('db:seed', { dbName: testUserDb, seedName: 'twoTrees' })

    cy.request('POST', config.TEST_SERVER + '/logout')
    cy.clearCookie('AuthSession')

    cy.login(testEmail)
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should have working Upgrade modal', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])

    cy.get('#account')
      .click()
      .get('#upgrade-button')
      .click()

    cy.contains('Upgrade Gingko Writer')

    cy.get('#currency-selector')
      .should('contain', 'Select your currency')
      .select('usd')

    cy.get('#upgrade-checkout')
      .should('contain', '$10/mo')

    // Modal closes correctly
    cy.get('.close-button').click()
    cy.get('.modal').should('not.exist')

    // Modal persists Upgrade model
    cy.get('#account')
      .click()
      .get('#upgrade-button')
      .click()

    cy.get('#currency-selector')
      .should('have.value', 'usd')
    cy.get('#upgrade-checkout')
      .should('contain', '$10/mo')

    // Correct priceId set
    let expectedData = {currency: "usd", billing: "monthly", plan: "regular"}
    cy.get('.modal-guts button')
      .click()

    cy.window().then((win) => {
      expect(win.elmMessages.slice(-1)[0]).to.eq({tag: "CheckoutButtonClicked", data: expectedData})
    })
  })
})
