const Application = require('spectron').Application
const {expect} = require('chai')
const electronPath = require('electron') // Require Electron from the binaries included in node_modules.
const path = require('path')
const robot = require('robotjs')

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

  it('should show dev tools when going to "Help > Show Dev Tools"', async function () {
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
      args: [path.join(__dirname, '../app')]
    })
    return app.start().then(function (result) {
      client = app.client
    })
  })

  // Close app after all tests have run.
  // Hack to click "Close without saving" in "Save changes?" dialog.
  // Platform and distro dependant!
  after(async function () {
    if (app && app.isRunning()) {
      app.stop()
      robot.moveMouse(818, 561)
      await client.pause(500)
      robot.mouseClick()
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
