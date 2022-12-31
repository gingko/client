const config = require("../../config.js");


describe('Managing Documents', function () {
  const testEmail = 'cypress@testing.com'

  beforeEach(() => {
    cy.deleteUser(testEmail).then(() => {
      cy.signup_with(testEmail, 'fourSmallTrees')
      cy.fixture('fourSmallTrees.ids.json').as('treeIds')
    })
  })

  it('Navigates correctly', function () {
    describe('Should navigate to last edited tree', function () {
      cy.visit(config.TEST_SERVER)

      cy.get('@treeIds').then((treeIds) => {
        cy.url().should('contain', treeIds[1] )
      });

      cy.get('.spinner', {timeout: 20000}).should('not.exist')

      cy.contains('123')

      cy.get('#documents-icon').click()

      cy.get('#sidebar-document-list-wrap .sidebar-document-item')
        .should('have.length', 4)

      cy.get('#sidebar-document-list-wrap .sidebar-document-item.active')
        .contains('tree-1')

      cy.get('#title').contains('tree-1')

      // Prevent navigating away if editing
      const stub = cy.stub()
      cy.on('window:alert', stub)

      cy.shortcut('{enter}')
      cy.writeInCard('chhh')
      cy.get('#sidebar-document-list-wrap .sidebar-document-item')
        .last()
        .click()
        .then(()=> {
          expect(stub.getCall(0)).to.be.calledWith('You have unsaved changes!\nCtrl+Enter to save.')
        })
      cy.get('@treeIds').then((treeIds) => {
        cy.url().should('contain', treeIds[1] )
      });
    })

    describe('Should have working sidebar and Quick Switcher', function () {
      cy.get('@treeIds').then((treeIds) => {
        cy.visit(config.TEST_SERVER + '/' + treeIds[1]);
        cy.url().should('contain', treeIds[1])
      })

      // Open sidebar
      cy.get('.spinner', {timeout: 20000}).should('not.exist')
      //cy.get('#documents-icon', {timeout: 20000}).click()

      cy.contains('#sidebar-document-list-wrap', 'tree-u', {timeout: 20000})
        .contains('#sidebar-document-list-wrap', 'tree-a')

      // Go to uvw doc
      cy.get('#sidebar-document-list-wrap').contains('tree-u').click()
      cy.contains('#document', 'uvw')

      // Got to another doc
      cy.get('#sidebar-document-list-wrap').contains('tree-a')
        .click()
      cy.get('@treeIds').then((treeIds) => {
        cy.url().should('contain', treeIds[0] )
      })
      cy.contains('abc')

      describe('Sorts documents correctly', ()=>{
        cy.get('#sidebar-document-list .sidebar-document-item a')
          .then((docEls) => {
            expect(docEls.toArray().map(x => x.innerText)).to.deep.equal(['tree-1','tree-u','tree-a','tree-x'])
          })

        cy.get('#sort-alphabetical').click()
        cy.get('#sidebar-document-list .sidebar-document-item a')
          .then((docEls) => {
            expect(docEls.toArray().map(x => x.innerText)).to.deep.equal(['tree-1','tree-a','tree-u','tree-x'])
          })

        cy.get('#sort-created').click()
        cy.get('#sidebar-document-list .sidebar-document-item a')
          .then((docEls) => {
            expect(docEls.toArray().map(x => x.innerText)).to.deep.equal(['tree-x','tree-a','tree-u','tree-1'])
          })

        cy.get('#sort-modified').click()
        cy.get('#sidebar-document-list .sidebar-document-item a')
          .then((docEls) => {
            expect(docEls.toArray().map(x => x.innerText)).to.deep.equal(['tree-1','tree-u','tree-a','tree-x'])
          })
      })

      describe('Filters by name correctly in sidebar', () => {
        cy.get('#document-list-filter').type('-u')

        cy.get('#sidebar-document-list-wrap .sidebar-document-item', {timeout: 20000})
          .should('have.length', 1)
          .should('contain', 'tree-u')
          .should('not.contain', 'tree-a')
          .should('not.contain', 'tree-1')

        cy.get('#document-list-filter').type('x')

        cy.get('#no-documents')
          .should('be.visible')

        cy.get('#sidebar-document-list-wrap .sidebar-document-item', {timeout: 20000})
          .should('not.exist')

        cy.get('#document-list-filter').type('{selectall}{backspace}')

        cy.get('#sidebar-document-list-wrap .sidebar-document-item', {timeout: 20000})
          .should('have.length', 4)
      })

      // Should have a working context menu
      // Menu opens on right click
      cy.get('#sidebar-document-list-wrap .sidebar-document-item:nth-child(2)')
        .rightclick()

      cy.contains('Delete Tree')

      // Should close context menu on clicking elsewhere
      cy.get('#sidebar-context-overlay').click()
      cy.get('#sidebar-context-menu').should('not.exist')

      // Open menu again
      cy.get('#sidebar-document-list-wrap .sidebar-document-item:nth-child(2)')
        .rightclick()

      // Click the Delete Tree
      cy.contains('Delete Tree')
        .click()

      // Document should be deleted
      cy.get('#sidebar-document-list-wrap .sidebar-document-item', {timeout: 20000})
        .should('have.length', 3)
        .should('not.contain', 'tree-u')

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
            .to.eql(['tree-a', 'tree-1', 'tree-x'])
        })

        cy.get('#switcher-modal .switcher-document-list .switcher-document-item')
          .first()
          .should('have.class', 'current')
          .should('have.class', 'selected')

        // Check list navigation
        cy.shortcut('{downarrow}{downarrow}{uparrow}')
          .get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
            expect($list[0]).to.have.class('current').and.to.not.have.class('selected')
            expect($list[1]).to.have.class('selected')
            expect($list[2]).to.not.have.class('selected')
          })

        // Test filtering
        cy.get('#switcher-modal input').type('x')

        cy.get('#switcher-modal .switcher-document-list')
          .should('contain', 'tree-x')
          .should('not.contain', 'tree-a')
          .should('not.contain', 'tree-1')

        // Should close on esc
        cy.shortcut('{esc}')
        cy.get('#switcher-modal').should('not.exist')
        cy.contains('abc')


        // Should go to selected tree on {enter}
        cy.shortcut('{ctrl}o')
        cy.shortcut('{downarrow}')
        cy.shortcut('{enter}')
        cy.get('@treeIds').then((treeIds) => {
          cy.url().should('contain', treeIds[1] )
        })


        // Should select first tree when filtering
        cy.shortcut('{ctrl}o')
        cy.get('#switcher-modal').should('exist')
        cy.get('#switcher-modal input').type('x')

        cy.get('#switcher-modal .switcher-document-list .switcher-document-item').then($list => {
          expect($list[0]).to.have.class('selected').and.to.not.have.class('current')
          expect($list).to.have.length(1)
        })
        cy.shortcut('{enter}')
        cy.get('@treeIds').then((treeIds) => {
          cy.url().should('contain', treeIds[2] )
        })

        cy.contains('xyz')
      })
    })
  })
})
