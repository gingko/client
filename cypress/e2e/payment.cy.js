const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");

Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}

describe('Payment Tests', () => {
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
    cy.get('#app-root').should('be.visible')
    cy.get('.spinner').should('not.exist')

    cy.get("#upgrade-cta")
      .should('not.contain', 'Trial Expired')

    cy.request('POST', config.TEST_SERVER + '/test/expired')

    cy.get("#upgrade-cta")
      .should('contain', 'Trial Expired')

    // Prevent editing in expired trial
    const stub = cy.stub()
    cy.on('window:alert', stub)
    cy.getCard(1,1,1).click()
    cy.shortcut('{enter}')
      .then(() => {
        expect(stub.getCall(0)).to.be.calledWith('Trial Expired')
      })


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

    // Can edit again after upgrade
    cy.getCard(1,1,1).click()
    cy.shortcut('{enter}')
    cy.get('textarea').should('have.value', 'Another Test doc')
  })
})
