const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


describe('Managing Documents', () => {
  const testEmail = 'cypress@testing.com'
  const testUserDb = 'userdb-' + helpers.toHex(testEmail);

  before(() => {
    cy.deleteUser(testEmail)
    cy.signup_blank(testEmail)

    cy.task('db:seed',{dbName: testUserDb, seedName: 'twoTrees'})

    cy.request('POST', config.TEST_SERVER + '/logout')
    cy.clearCookie('AuthSession')

    cy.wait(400)
    cy.login(testEmail)
  })

  beforeEach(() => {
    cy.fixture('twoTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  describe('Navigation', function () {
    it('Should navigate to last edited tree', function () {
      cy.visit(config.TEST_SERVER)

      cy.url().should('contain', this.treeIds[1] )
    })

    it('Should show the trees in sidebar document list', () => {
      cy.get('#file-button').click()

      cy.contains('#sidebar-menu', 'Untitled')
        .contains('#sidebar-menu', 'Another doc, with title')
    })

    it('Should go to the first tree on click', () => {
      cy.contains('Untitled')
        .click()

      cy.contains('#document', 'Hello Test doc')
        .contains('#document', 'Child card')
        .contains('#document', 'Another Child card')
    })

    it('Should navigate to tree on clicking sidebar document', function () {
      cy.get('#sidebar-menu').contains('Another').click()

      cy.url().should('contain', this.treeIds[1] )
    })

    it('Should bring up context menu on right-click of tree', () => {
      cy.get('#sidebar-menu .sidebar-document-item')
        .first()
        .rightclick()

      cy.contains('Delete Tree')
    })
  })

  describe('Quick Switcher', ()=>{
    it('Toggles switcher modal on "Ctrl+O"', () => {
      cy.get('#switcher-modal').should('not.exist')

      cy.shortcut('{ctrl}o')
      cy.get('#switcher-modal').should('exist')

      cy.shortcut('{ctrl}o')
      cy.get('#switcher-modal').should('not.exist')
    })

    it('It autofocuses on switcher modal input', () => {
      cy.shortcut('{ctrl}o')

      cy.get('#switcher-modal input').should('have.focus')
    })

    it('Displays list of trees in switcher', () => {
      cy.get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
        expect($list[0].innerHTML).to.contain('Another doc, with title')
        expect($list[1].innerHTML).to.contain('Untitled')
      })
    })

    it('Filters list of trees', ()=> {
      cy.get('#switcher-modal input').type('ano')

      cy.get('#switcher-modal .switcher-document-list')
        .should('contain', 'Another doc, with title')
        .should('not.contain', 'Untitled')
    })

    it('Closes "Open" modal on "Esc"', () => {
      cy.shortcut('{esc}')
      cy.get('#switcher-modal').should('not.exist')
    })

    it('Clears search filtering after "Esc"', () => {
      cy.contains('Untitled')
    })
  })
})
