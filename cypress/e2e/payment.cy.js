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
  })

  it('Should have working Upgrade modal', function () {
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.url().should('contain', this.treeIds[0] )
    cy.get('.spinner').should('not.exist')

    cy.get("#upgrade-cta")
      .should('not.contain', 'Trial Expired')

    cy.get('#upgrade-button')
      .click()

    cy.contains('Upgrade Gingko Writer')

    cy.get('#currency-selector')
      .should('contain', 'USD')

    cy.get('#upgrade-checkout')
      .should('contain', '$12.75')

    // Modal closes correctly
    cy.get('.close-button').click()
    cy.get('.modal').should('not.exist')

    // Modal persists Upgrade model
    cy.get('#upgrade-button')
      .click()

    cy.get('#currency-selector')
      .should('have.value', 'USD')
    cy.get('#upgrade-checkout')
      .should('contain', '$12.75')

    // Change billing frequency
    cy.get('input#yearly')
      .click()

    cy.contains("$117")
    cy.contains("per year")

    cy.wait(500)

    // Change in currency should reflect in displayed price
    cy.get('#currency-selector')
      .select('INR')
    cy.contains('₹2400')
  })

  it('Correctly handles payment status', function () {
    cy.visit(config.TEST_SERVER + '/upgrade/success' )

    cy.fixture('stripeSuccess').then((json) => {
      cy.request('POST', config.TEST_SERVER+'/hooks',json);
    })

    cy.get('.message-cta')
      .click()

    cy.url().should('contain', this.treeIds[1] )
    cy.get('.spinner').should('not.exist')

    cy.contains('Synced')
    cy.contains('Another doc, with title')
    cy.get('#document-header')
      .should('not.contain', 'Upgrade')
  })
})
