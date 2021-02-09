const config = require("../../config.js");
const helpers = require("../../src/shared/doc-helpers.js");


Cypress.LocalStorage.clear = function (keys, ls, rs) {
  return;
}


describe('Managing Documents', () => {
  const testEmail = 'cypress@testing.com'

  before(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'sevenTrees')
    })
  })

  beforeEach(() => {
    cy.fixture('sevenTrees.ids.json').as('treeIds')
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  describe('Navigation', function () {
    it('Should navigate to last edited tree', function () {
      cy.visit(config.TEST_SERVER)

      cy.url().should('contain', this.treeIds[6] )

      cy.get('#file-button', {timeout: 20000}).click()

      cy.get('#sidebar-menu .sidebar-document-item', {timeout: 0})
        .should('have.length', 7)


      cy.get('#title').contains('Random letters')

    })

    it('Should have working sidebar and Quick Switcher', function () {
      cy.visit(config.TEST_SERVER + '/' + this.treeIds[2]);
      cy.url().should('contain', this.treeIds[2] )

      // Open sidebar
      cy.get('#file-button', {timeout: 20000}).click()

      cy.contains('#sidebar-menu', 'welcome', {timeout: 20000})
        .contains('#sidebar-menu', 'timeline 2021')

      // Go to Welcome doc
      cy.get('#sidebar-menu').contains('welcome').click()

      // Check Welcome doc contents
      cy.contains('#document', 'Welcome to Gingko Writer')
        .contains('#document', 'Adding New Cards')
        .contains('#document', 'Moving Cards')

      // Got to another doc
      cy.get('#sidebar-menu').contains('Screenplay')
        .click()
      cy.url().should('contain', this.treeIds[5] )

      cy.contains('tips to improve your logline')

      // Should have a working context menu
      // Menu opens on right click
      cy.get('#sidebar-menu .sidebar-document-item')
        .first()
        .rightclick()

      cy.contains('Delete Tree')

      // Should close context menu on clicking elsewhere
      cy.get('#sidebar-context-overlay').click()
      cy.get('#sidebar-context-menu').should('not.exist')

      // Open menu again
      cy.get('#sidebar-menu .sidebar-document-item')
        .first()
        .rightclick()

      // Click the Delete Tree
      cy.contains('Delete Tree')
        .click()

      // Document should be deleted
      cy.get('#sidebar-menu .sidebar-document-item', {timeout: 20000})
        .should('have.length', 6)
        .should('not.contain', 'Romeo and Juliet')

      // And menu should be gone
      cy.get('#sidebar-context-menu').should('not.exist')

      describe('Quick Switcher', () => {
        // Toggles switcher modal on "Ctrl+O"
        // Check for toggling and autofocus
        cy.get('#switcher-modal').should('not.exist')

        cy.shortcut('{ctrl}o')
        cy.get('#switcher-modal').should('exist')
        cy.get('#switcher-modal input').should('have.focus')

        cy.shortcut('{ctrl}o')
        cy.get('#switcher-modal').should('not.exist')

        // Check contents
        cy.shortcut('{ctrl}o')

        cy.get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
          expect($list.toArray().map(li => li.innerText))
            .to.eql(['Screenplay', 'Random letters', 'timeline 2021', 'Timeline 2019/2020', 'welcome', 'Example Tree'])
        })

        cy.get('#switcher-modal .switcher-document-list .switcher-document-item')
          .first()
          .should('have.class', 'current')
          .should('have.class', 'selected')

        // Check list navigation
        cy.shortcut('{downarrow}{downarrow}{downarrow}{uparrow}')
          .get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
            expect($list[0]).to.have.class('current').and.to.not.have.class('selected')
            expect($list[1]).to.not.have.class('selected')
            expect($list[2]).to.have.class('selected')
            expect($list[3]).to.not.have.class('selected')
          })

        // Test filtering
        cy.get('#switcher-modal input').type('exa')

        cy.get('#switcher-modal .switcher-document-list')
          .should('contain', 'Example Tree')
          .should('not.contain', 'Screenplay')

        // Should close on esc
        cy.shortcut('{esc}')
        cy.get('#switcher-modal').should('not.exist')
        cy.contains('Screenplay')


        // Should go to selected tree on {enter}
        cy.shortcut('{ctrl}o')
        cy.shortcut('{downarrow}')
        cy.shortcut('{downarrow}')
        cy.shortcut('{downarrow}')
        cy.shortcut('{enter}')
        cy.url().should('contain', this.treeIds[4] )


        // Should select first tree when filtering
        cy.shortcut('{ctrl}o')
        cy.get('#switcher-modal').should('exist')
        cy.get('#switcher-modal input').type('welc')

        cy.get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
          expect($list[0]).to.have.class('selected').and.to.not.have.class('current')
          expect($list).to.have.length(1)
        })
        cy.shortcut('{enter}')
        cy.url().should('contain', this.treeIds[1] )

        cy.contains('Welcome to Gingko Writer')
      })
    })
  })
})
