const config = require("../../config.js");
const { tr } = require("../../src/shared/translation.js");

describe('Document Editing', () => {
  const testEmail = 'cypress@testing.com'

  beforeEach(() => {
    cy.deleteUser(testEmail).then(()=>{
      cy.signup_with(testEmail, 'oneEmptyTree').then(()=>{
        cy.visit(config.TEST_SERVER)
      })
    })
    cy.fixture('oneTree.ids.json').as('treeIds')
  })

  it('Can edit a git-like document',{retries: {runMode: 2, openMode: 0}}, function(){
    cy.url().should('contain', this.treeIds[0] )

    cy.contains('Untitled')

    // Can edit and save card
    cy.getCard(1,1,1).click()
    cy.wait(250)
    cy.shortcut('{enter}')

    cy.get('textarea').should('have.focus')
      .type('Hello World :)')

    cy.shortcut('{ctrl}{enter}')

    cy.getCard(1,1,1).get('.view').contains('Hello World :)')

    cy.contains('Synced')

    // Create a new child on clicking right + button
    cy.getCard(1,1,1).trigger('mouseover')

    cy.get('.ins-right').click()

    cy.get('textarea').should('have.focus')
      .type('A child')

    // Saves the card by clicking the checkmark
    cy.get('.card-btn')
      .click()

    cy.get('div.card.active')
      .contains('A child')

    // Clicking on a different card while editing doesn't
    // make them both have the same content (bug)
    cy.getCard(1,1,1).click()
    cy.shortcut('{enter}')
    cy.writeInCard('XYZ')
    cy.getCard(2,1,1).click()
    cy.getCard(2,1,1).should('not.contain', 'XYZ')
    cy.getCard(1,1,1).should('contain', 'XYZ')

    // Clicking outside a card while editing should save that card
    cy.getCard(1,1,1).click()
    cy.shortcut('{enter}')
    cy.writeInCard('UVW')
    cy.get('.left-padding-column').click()
    cy.get('#save-indicator').should('not.contain', 'Unsaved Changes...')
    cy.getCard(1,1,1)
      .should('not.have.class', 'editing')
      .should('contain', 'UVW')

    // Create and saves a card below using shortcuts
    cy.shortcut('l')
    cy.shortcut('{ctrl}j')

    cy.get('textarea').should('have.focus')
      .type('Another one below')

    cy.shortcut('{ctrl}{enter}')

    cy.get('div.card.active')
      .contains('Another one below')

    cy.contains('Synced')

    // Saves data with correct author info
    cy.window().then((win) => {
      console.log(win.elmMessages)
      let {tag, data} = win.elmMessages.filter(m => m.tag === "CommitData").slice(-1)[0];

      expect(tag).to.eq("CommitData")

      expect(data.author)
        .to.eq(`<${testEmail}>`)
    })

    // Cancels changes correctly after confirmation
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

    // Can cancel renaming the document
    cy.get('#title-rename').click({force: true}) // force hack : Upgrade button covers it only on headless runs
    cy.shortcut('{esc}')
    cy.get('#title-rename').should('not.be.focused')

    // Can rename the document
    cy.get('#title-rename').click({force: true}) // force hack : Upgrade button covers it only on headless runs
    cy.get('#title-rename')
      .should('have.focus')
      .type('A new doc title here{enter}')

    cy.title().should('eq', 'A new doc title here - Gingko Writer')
    cy.get('#title-rename').should('have.value', 'A new doc title here')

    cy.wait(400)
    cy.reload()

    cy.getCard(2,1,2)
      .contains('Another one below')

    // Has saved the activation state
    cy.wait(400)
    cy.getCard(1,1,1)
      .should('have.class', 'ancestor')

    cy.getGroup(2,1)
      .should('have.class', 'has-active')
      .should('not.have.class', 'active-descendant')

    cy.getCard(2,1,2)
      .should('have.class', 'active')

    // Filters cards on search
    cy.get('#search-field')
      .click()
      .type('another')

    cy.get('#document')
      .should('not.contain', 'Hello World :)')

    // Removes filters on clearing search
    cy.get('#search-input')
      .clear()

    cy.contains('Hello World :)')

    // Can move back to last change
    cy.shortcut('{ctrl}z')

    cy.contains('Restore this Version')

    cy.get('#app-root')
      .should('not.contain', 'Another one below')

    // Restores last change
    cy.get('#history-restore').click()

    cy.get('#app-root')
      .should('not.contain', 'Another one below')

    cy.get('#history-menu').should('not.exist')

    cy.wait(500)

    // Can split card down
    cy.shortcut('{enter}')
    cy.shortcut('{leftarrow}{leftarrow}{leftarrow}{leftarrow}{leftarrow}{leftarrow}{leftarrow}')
    cy.shortcut('{ctrl}j')
    cy.get('textarea')
      .should('have.value', ')XYZUVW')
    cy.getCard(1,1,1)
      .should('not.contain',')XYZUVW')
    cy.shortcut('{ctrl}{enter}')

    // Correctly splits card up
    cy.shortcut('{enter}')
    cy.shortcut('{leftarrow}{leftarrow}{leftarrow}')
    cy.shortcut('{ctrl}k')
    cy.get('textarea')
      .should('have.value', ')XYZ')
    cy.shortcut('{ctrl}{enter}')
    cy.getCard(1,1,2)
      .should('contain',')XYZ')
    cy.getCard(1,1,3)
      .should('contain','UVW')

    // Copy/Paste tests
    cy.getCard(2,1,1).click()
    cy.shortcut('{ctrl}c')
    cy.shortcut('{ctrl}v')
    cy.getCard(2,1,1).should('not.have.class','active').should('contain', 'A child')
    cy.getCard(2,1,2).should('have.class','active').should('contain', 'A child')

    // New card, to test title shortcuts
    cy.getCard(1,1,1).click()
    cy.shortcut('{ctrl}{rightArrow}')
    cy.writeInCard('A test title{enter}{enter}body')

    cy.writeInCard('{alt}1')
      .then(($el) => {
        expect($el).to.have.value('# A test title\n\nbody')

        cy.shortcut('{ctrl}{enter}')

        cy.get('div.view h1')
          .contains('A test title')
      })

    cy.shortcut('{enter}')
    cy.writeInCard('{alt}3')
      .then(($el) => {
        expect($el).to.have.value('### A test title\n\nbody')

        cy.shortcut('{ctrl}{enter}')

        cy.get('div.view h3')
          .contains('A test title');
      })

    cy.shortcut('{enter}')
    cy.writeInCard('{alt}0')
      .then(($el) => {
        expect($el).to.have.value('A test title\n\nbody')

        cy.shortcut('{ctrl}{enter}')

        cy.get('div.active p')
          .first()
          .should('contain', 'A test title')
      })

    // Test formatting shortcuts
    cy.get('textarea')
      .should('not.exist')
    cy.shortcut('{ctrl}{downarrow}')
    cy.writeInCard('bold')
    cy.shortcut('{selectall}')
    cy.shortcut('{ctrl}{b}')
    cy.get('textarea')
      .should('have.value', '**bold**')
    cy.shortcut('{ctrl}{enter}')
    cy.getCard(2,1,4)
      .should('contain.html','<strong>bold</strong>')
    cy.shortcut('{ctrl}{downarrow}')
    cy.writeInCard('italic')
    cy.shortcut('{selectall}')
    cy.shortcut('{ctrl}{i}')
    cy.get('textarea')
      .should('have.value', '*italic*')
    cy.shortcut('{ctrl}{enter}')
    cy.getCard(2,1,5)
      .should('contain.html','<em>italic</em>')

    // Test card merging
    cy.getCard(2,1,4).click()
    cy.shortcut('{ctrl}{shift}{downarrow}')
    cy.getCard(2,1,4)
      .should('contain.html', '<p><strong>bold</strong></p>\n<p><em>italic</em></p>')

    cy.getCard(1,1,2).click()
    cy.shortcut('{ctrl}{shift}{uparrow}')
    cy.getCard(1,1,1)
      .should('contain.html', '<p>Hello World :</p>\n<p>)XYZ</p>')

    cy.contains('Synced')
  })
})