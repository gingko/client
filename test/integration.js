const Application = require('spectron').Application
const {expect} = require('chai')
const electronPath = require('electron') // Require Electron from the binaries included in node_modules.
const path = require('path')

describe.skip('Application Lifecycle', function () {
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

  xit('shows an initial window', async function () {
    const windowCount = await client.getWindowCount()
    expect(windowCount).to.be.equal(1)
  })


  // Does not work.
  it('should show dev tools when going to "Help > Show Dev Tools"', async function () {
    client.pause(1000)
    await client.execute(async () => {
      var el = await app.electron
      var menu = app.electron.remote.Menu.getApplicationMenu()
      console.log(menu)
    })
  })


  // Does not work.
  it('should close the window when pressing Ctrl+Q', async function () {
    client.windowByIndex(0)
    await client.keys(['Control', 'Q'])
    client.pause(2000)
    const finalWindowCount = await client.getWindowCount()
    expect(finalWindowCount).to.be.equal(0)
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
  // Unfortunately, "Save changes?" dialog prevents this.
  after(function () {
    if (app && app.isRunning()) {
      return app.stop()
    }
  })

  it('should initial card', async function () {
    const cardExists = await client.isExisting('#card-1')
    expect(cardExists).to.be.true
  })

  it('should switch to edit mode when pressing Enter', async function () {
    await client.keys(['Enter'])
    const textareaExists = await client.waitForExist('#card-edit-1', 200)
    expect(textareaExists).to.be.true
  })

  it('should have text "Hello World" in card after typing it', async function () {
    await client.keys(["Hello World"])
    const textareaValue = await client.getValue('#card-edit-1')
    expect(textareaValue).to.equal("Hello World")
  })

  it('should switch to navigation mode when pressing Ctrl+Enter', async function () {
    const step1 = await client.keys(['Control', 'Enter'])
    const cardViewExists = await client.waitForExist('#card-1 .view', 120)
    expect(cardViewExists).to.be.true
  })

  it('should show "Hello World" in card view', async function () {
    const cardText = await client.getText('#card-1 .view')
    expect(cardText).to.equal("Hello World")
  })
})
