const config = require("../../config.js");


describe('Not Found (Logged In User)', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(()=>{
      cy.signup_with(testEmail, 'twoTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
  })

  it('Should go to not-found page', function () {
    cy.visit(config.TEST_SERVER+ '/aaaaa')
    cy.wait(250)

    cy.url().should('eq', config.TEST_SERVER + '/aaaaa/404-not-found')
    cy.contains('Hmm, we couldn\'t find this document')

    // Can still navigate to the other trees
    cy.visit(config.TEST_SERVER + '/' + this.treeIds[0])
    cy.contains('Hello Test doc')

    cy.visit(config.TEST_SERVER + '/' + this.treeIds[1])
    cy.contains('Another Test doc')
  })
})
