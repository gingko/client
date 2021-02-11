const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Upgrade process', () => {
  const testEmail = 'cypress@testing.com'
  const testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'twoTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Should have working Upgrade modal', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.url().should('contain', this.treeIds[0] )

    cy.get('#account')
      .click()
      .get('#upgrade-button')
      .click()

    cy.contains('Upgrade Gingko Writer')

    cy.get('#currency-selector')
      .should('contain', 'Select your currency')
      .select('USD')

    cy.get('#upgrade-checkout')
      .should('contain', '$10')

    // Modal closes correctly
    cy.get('.close-button').click()
    cy.get('.modal').should('not.exist')

    // Modal persists Upgrade model
    cy.get('#account')
      .click()
      .get('#upgrade-button')
      .click()

    cy.get('#currency-selector')
      .should('have.value', 'USD')
    cy.get('#upgrade-checkout')
      .should('contain', '$10')

    // Change billing frequency
    cy.get('input#yearly')
      .click()

    cy.contains("$99")
    cy.contains("per year")

    // Change in currency should reflect in displayed price
    cy.get('#currency-selector')
      .select('INR')
    cy.contains('₹2000')

    // Should toggle Pay What You Want pricing
    cy.get('#pwyw')
      .should('not.contain', 'Discount')

    cy.get('#pwyw-toggle')
      .click()

    cy.get('#pwyw')
      .should('contain', 'Discount')

    cy.get('#discount')
      .click()
      .should('have.class','checked')

    cy.get('#upgrade-checkout')
      .should('contain', '1000')
  })

  it('Shows trial days remaining', function () {
    cy.visit(config.TEST_SERVER)
    cy.url().should('contain', this.treeIds[1] )

    cy.get('#account')

    cy.get('#document-header').contains('30 days left')
  })

  it('Correctly handles payment status', function () {
    cy.visit(config.TEST_SERVER + '/upgrade/success' )

    cy.fixture('stripeSuccess').then((json) => {
      cy.request('POST', config.TEST_SERVER+'/hooks',json);
    })

    cy.get('.message-cta')
      .click()

    cy.url().should('contain', this.treeIds[1] )

    cy.get('#account')

    cy.get('#document-header')
      .should('not.contain', 'Upgrade')
  })
})
