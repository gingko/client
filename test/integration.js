const Application = require('spectron').Application
const {expect} = require('chai')
const electronPath = require('electron') // Require Electron from the binaries included in node_modules.
const path = require('path')
//const robot = require('robotjs')
const { execSync } = require('child_process')
const fs = require('fs')
const { unlink } = require('fs')
const { promisify } = require('util')


describe('Application Start', function () {
  const app = new Application({
        path: electronPath,
        args: [path.join(__dirname, "../app")]
      });

  this.timeout(10000)

  beforeEach(() => {
    return app.start();
  });

  afterEach(function () {
    if (app && app.isRunning()) {
      return app.stop()
    }
  });

  it("should work", () => {
    expect(true).to.be.true;
  });

  it("should display Home window", async () => {
    const count = await app.client.getWindowCount();
    const title = await app.client.waitUntilWindowLoaded().getTitle();
    expect(count).to.eq(1);
    return expect(title).to.eq("Gingko - Home Screen");
  });

  it("should create a document window on clicking 'Blank'", async () => {
    await app.client.waitUntilWindowLoaded();
    await app.client.click("#template-new");
    await app.client.pause(600);
    await app.client.windowByIndex(0);
    const count = await app.client.getWindowCount();
    const title = await app.browserWindow.getTitle();
    expect(count).to.eq(1);
    return expect(title).to.eq("Untitled - Gingko");
  });
});

describe("Document Actions", function () {
  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: '1'
          , DIALOG_CHOICE: 0 // Close Without Saving
          },
        args: [path.join(__dirname, "../app"), "new"]
      });

  this.timeout(10000)

  before(() => {
    return app.start();
  });

  after(function () {
    if (app && app.isRunning()) {
      return app.stop();
    }
  });

  it("should show initial card", async () => {
    const cardExists = await app.client.isExisting("#card-1");
    expect(cardExists).to.be.true;
  });

  it("should switch to edit mode when pressing Enter", async () => {
      await app.client.keys(["Enter"]);
      const textareaExists = await app.client.waitForExist("#card-edit-1", 800);
      expect(textareaExists).to.be.true;
  });

  it("should have text \"Hello World\" in card after typing it", async () => {
    await app.client.keys(["Hello World"]);
    const textareaValue = await app.client.getValue('#card-edit-1');
    expect(textareaValue).to.equal("Hello World");
  })
});
