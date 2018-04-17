const Application = require('spectron').Application
const {expect} = require('chai')
const electronPath = require('electron') // Require Electron from the binaries included in node_modules.
const path = require('path')
const robot = require('robotjs')
const { execSync } = require('child_process')
const fs = require('fs')
const { unlink } = require('fs')
const { promisify } = require('util')

describe('Application Start', function () {
  this.timeout(10000)

  var app, client

  beforeEach(function () {
    app = new Application({
      path: electronPath,
      args: [path.join(__dirname, '../app')]
    })
    return app.start().then(function (result) {
      client = app.client
    })
  })

  afterEach(function () {
    if (app && app.isRunning()) {
      return app.stop()
    }
  })

  it('shows an initial window', async function () {
    const windowCount = await client.getWindowCount()
    expect(windowCount).to.be.equal(1)
  })

  it('has title "Untitled Tree - Gingko"', async function () {
    let windowTitle = await app.browserWindow.getTitle()
    expect(windowTitle).to.equal("Untitled Tree - Gingko")
  })

  it('shows dev tools when going to "Help > Show Dev Tools"', async function () {
    robot.keyTap('h', 'alt')
    robot.keyTap('d')
    await client.pause(200)
    const windowCount = await client.getWindowCount()
    expect(windowCount).to.be.equal(2)
  })
})


describe('Application Exit', function () {
  this.timeout(10000)

  var app, client

  beforeEach(function () {
    app = new Application({
      path: electronPath,
      args: [path.join(__dirname, '../app')],
      quitTimeout: 10
    })
    return app.start().then(function (result) {
      client = app.client
    })
  })

  // Skip. 'chrome not reachable' error.
  xit('should close the window when pressing Ctrl+Q', async function () {
    robot.keyTap('q', 'control')
    await client.pause(200)
    const windowCount = await client.getWindowCount()
    expect(app.isRunning()).to.be.false
  })
})


describe('Basic Actions', function () {
  this.timeout(10000)

  var app, client

  before(function () {
    app = new Application({
      path: electronPath,
      env:
        { RUNNING_IN_SPECTRON: '1'
        , DIALOG_CHOICE: 0 // Close Without Saving
        },
      args: ['-r', path.join(__dirname, 'mocks.js'), path.join(__dirname, '../app')]
    })
    return app.start().then(function (result) {
      client = app.client
    })
  })

  // Close app after all tests have run.
  after(async function () {
    if (app && app.isRunning()) {
      await app.stop()
    }
  })

  it('should initial card', async function () {
    const cardExists = await client.isExisting('#card-1')
    expect(cardExists).to.be.true
  })

  it('should switch to edit mode when pressing Enter', async function () {
    await client.keys(['Enter'])
    const textareaExists = await client.waitForExist('#card-edit-1', 800)
    expect(textareaExists).to.be.true
  })

  it('should have text "Hello World" in card after typing it', async function () {
    await client.keys(["Hello World"])
    const textareaValue = await client.getValue('#card-edit-1')
    expect(textareaValue).to.equal("Hello World")
  })

  it('should have title "*Untitled Tree - Gingko"', async function () {
    let windowTitle = await app.browserWindow.getTitle()
    expect(windowTitle).to.equal("*Untitled Tree - Gingko")
  })

  it('should switch to navigation mode when pressing Ctrl+Enter', async function () {
    const step1 = await client.keys(['Control', 'Enter'])
    const cardViewExists = await client.waitForExist('#card-1 .view', 800)
    expect(cardViewExists).to.be.true
  })

  it('should show "Hello World" in card view', async function () {
    const cardText = await client.getText('#card-1 .view')
    expect(cardText).to.equal("Hello World")
  })
})


describe('Close Confirmations', function () { // Close Without Saving
  this.timeout(10000)

  describe('Close Without Saving', function () {
    let dialogChoice = 0 // Close Without Saving
    let filepath = path.join(__dirname, 'testfile-close-confirmation-open.gko')
    var app, client

    beforeEach(function () {
      app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: '1'
          , DIALOG_CHOICE: dialogChoice
          , DIALOG_OPEN_PATH: filepath
          },
        args: ['-r', path.join(__dirname, 'mocks.js'), path.join(__dirname, '../app')],
        quitTimeout: 10
      })
      return app.start().then(async function (result) {
        client = app.client
        await client.keys(['Enter']) // Enter Edit mode
        await client.waitForExist('#card-edit-1', 800) // Wait for Edit mode
        await client.keys(["Hello World"]) // Type something
        await client.pause(800)
      })
    })

    afterEach(function () {
      if (app && app.isRunning()) {
        return app.stop()
      }
    })

    describe('Exit', function () {
      it('should discard the changes and close the app', async function(){
        // Send Exit command, should trigger dialog
        // Choice 0 = "Close Without Saving"
        await app.stop()
        expect(app.isRunning()).to.be.false
      })
    })

    describe('New', function () {
      beforeEach(async function() {
        await client.windowByIndex(0)
        robot.keyTap('n', 'control')
      })

      it('should discard the changes', async function() {
        await client.waitForExist('#card-1 .view', 500)
        let cardText = await client.getText('#card-1 .view')
        expect(cardText).to.equal("")
      })

      it('should reset the title', async function() {
        let windowTitle = await app.browserWindow.getTitle()
        expect(windowTitle).to.equal("Untitled Tree - Gingko")
      })
    })

    describe('Open', function() {
      beforeEach(async function() {
        await client.windowByIndex(0)
        robot.keyTap('o', 'control')
      })

      it('should discard the changes', function() {
        // TODO: add a test file with known content
        //       and then check to see that that content is present
        let checkTextarea = async function() {
          await client.waitForExist('#card-edit-1', 500, true)
        }
        expect(checkTextarea).to.not.throw
      })

      it('should set the title based on loaded file', async function() {
        let windowTitle = await app.browserWindow.getTitle()
        expect(windowTitle).to.equal(`${path.basename(filepath)} - Gingko`)
      })
    })
    it('should discard the changes and import requested file')
  })

  describe('Cancel', function () {
    let dialogChoice = 1 // Cancel
    var app, client

    beforeEach(function () {
      app = new Application({
        path: electronPath,
        env: { RUNNING_IN_SPECTRON: '1', DIALOG_CHOICE: dialogChoice },
        args: ['-r', path.join(__dirname, 'mocks.js'), path.join(__dirname, '../app')]
      })
      return app.start().then(async function (result) {
        client = app.client
        await client.keys(['Enter']) // Enter Edit mode
        await client.waitForExist('#card-edit-1', 800) // Wait for Edit mode
        await client.keys(["Hello World"]) // Type something
      })
    })

    afterEach(function () {
      if (app && app.isRunning()) {
        return app.stop().then( () => {
          execSync('pkill electron; pkill electron')
        })
      }
    })

    describe('Exit', function () {
      it('should not close', async function(){
        // Send Exit command, should trigger dialog
        // Choice 1 = "Cancel"
        robot.keyTap('q', 'control')
        const textareaValue = await client.getValue('#card-edit-1')
        expect(textareaValue).to.be.equal("Hello World")
      })
    })

    describe('New', function () {
      beforeEach(async function() {
        await client.windowByIndex(0)
        robot.keyTap('n', 'control')
      })

      it('should not discard the changes', async function() {
        const textareaValue = await client.getValue('#card-edit-1')
        expect(textareaValue).to.be.equal("Hello World")
      })

      it('should not change the title', async function() {
        await client.pause(500)
        let windowTitle = await app.browserWindow.getTitle()
        expect(windowTitle).to.equal(`*Untitled Tree - Gingko`)
      })
    })

    describe('Open', function () {
      beforeEach(async function() {
        await client.windowByIndex(0)
        robot.keyTap('o', 'control')
      })

      it('should not discard the changes', async function() {
        const textareaValue = await client.getValue('#card-edit-1')
        expect(textareaValue).to.be.equal("Hello World")
      })


      it('should not change the title', async function() {
        await client.pause(500)
        let windowTitle = await app.browserWindow.getTitle()
        expect(windowTitle).to.equal(`*Untitled Tree - Gingko`)
      })
    })

    it('should not import requested file')
  })

  describe('Save', function () {
    let dialogChoice = 2 // Save
    let filepath = path.join(__dirname, 'testfile-close-confirmation-save.gko')
    var app, client

    beforeEach(function () {
      app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: '1'
          , DIALOG_CHOICE: dialogChoice
          , DIALOG_SAVE_PATH: filepath
          },
        args: ['-r', path.join(__dirname, 'mocks.js'), path.join(__dirname, '../app')]
      })
      return app.start().then(async function (result) {
        client = app.client
        await client.keys(['Enter']) // Enter Edit mode
        await client.waitForExist('#card-edit-1', 800) // Wait for Edit mode
        await client.keys(["Hello World"]) // Type something
      })
    })

    // Doesn't exit properly
    afterEach(function (done) {
      fs.unlink(filepath, function(err) {
        if (err) return done(err)
        done()
      })
    })

    it('should save the changes and exit', async function(){
      // Send Exit command, should trigger dialog
      // Choice 2 = "Save"
      robot.keyTap('f', 'alt')
      await client.pause(40)
      robot.keyTap('x')

      await client.pause(800)

      let checkfile = function() {
        fs.accessSync(filepath)
      }
      expect(checkfile).to.not.throw()
    })


    it('should save the changes and create a new file')
    it('should save the changes and load requested file')
    it('should save the changes and import requested file')
  })
})
