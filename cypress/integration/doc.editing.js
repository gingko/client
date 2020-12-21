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
    cy.visit(config.TEST_SERVER)
  })

  beforeEach(() => {
    Cypress.Cookies.preserveOnce('AuthSession')
  })

  it('Creates a new blank tree', () => {
    cy.get('button')
      .click()

    cy.contains('Blank Tree')
      .click()

    cy.url().as('testTreeUrl').should('match', /\/[a-zA-Z0-9]{5}$/)

    cy.contains('Untitled')
    cy.contains('New Document...')
  })

  it('Can edit and save card', () => {
    cy.get('textarea').should('have.focus')
      .type('Hello World :)')

    cy.get('body').type('{ctrl}{enter}')

    cy.get('#card-1 .view').contains('Hello World :)')
  })

  it('Is marked as "Synced"', () => {
    cy.contains('Synced')
  })

  it('Create a new child on clicking right + button', () => {
    cy.get('#card-1').trigger('mouseover')

    cy.get('.ins-right').click()

    cy.get('textarea').should('have.focus')
      .type('A child')
  })

  it('Saves the card by clicking the checkmark', () => {
    cy.get('.card-btn')
      .click()

    cy.get('div.card.active')
      .contains('A child')
  })

  it('Create and saves a card below using shortcuts', () => {
    cy.get('body').type('{ctrl}j')

    cy.get('textarea').should('have.focus')
      .type('Another one below')

    cy.get('body').type('{ctrl}{enter}')

    cy.get('div.card.active')
      .contains('Another one below')

    cy.contains('Synced')
  })

  it('Saves data with correct author info', () => {
    cy.window().then((win) => {
      console.log(win.elmMessages)
      let {tag, data} = win.elmMessages.filter(m => m.tag === "SaveData").slice(-1)[0];

      expect(tag).to.eq("SaveData")

      let lastCommit = _.sortBy(data.filter(d => d.type === 'commit'), 'timestamp').reverse()[0];
      expect(lastCommit.author)
        .to.eq(`<${testEmail}>`)
    })
  })

  it('Cancels changes correctly after confirmation', () => {
    let confirmCalled
    cy.on('window:confirm', (str) => {
      expect(str).to.eq(tr.areYouSureCancel["en"])
      confirmCalled = true
    })
    cy.shortcut('{enter}')
    cy.writeInCard(' changes to cancel xxx')
    cy.shortcut('{esc}')
    cy.should(() => {
      expect(confirmCalled).to.be.true
    })
    cy.get('#app-root')
      .should('not.contain', 'to cancel xxx')
  })

  it('Can rename the document', function () {
    cy.get('#title h1').click()
    cy.get('#title input')
      .should('have.focus')
      .type('A new doc title here')

    cy.contains('Rename')
      .click()

    cy.get('#title h1').contains('A new doc title here')
  })

  it('Has saved the content', function () {
    cy.wait(400)
    cy.visit(this.testTreeUrl)
    cy.getCard(2,1,2)
      .contains('Another one below')
  })

  it('Has saved the activation state', () => {
    cy.wait(400)
    cy.get('#card-1')
      .should('have.class', 'ancestor')

    cy.getGroup(2,1)
      .should('have.class', 'has-active')
      .should('not.have.class', 'active-descendant')

    cy.getCard(2,1,2)
      .should('have.class', 'active')
  })

  it('Filters cards on search', () => {
    cy.get('#search-field')
      .click()
      .type('another')

    cy.get('#app-root')
      .should('not.contain', 'Hello World :)')
  })

  it('Removes filters on clearing search', () => {
    cy.get('#search-input')
      .clear()

    cy.contains('Hello World :)')
  })

  it('Can move back to last change', () => {
    cy.shortcut('{ctrl}z')

    cy.contains('Restore this Version')

    cy.get('#app-root')
      .should('not.contain', 'Another one below')
  })

  it('Restores last change', () => {
    cy.get('#history-restore').click()

    cy.get('#app-root')
      .should('not.contain', 'Another one below')

    cy.get('#history').should('not.exist')
  })
})