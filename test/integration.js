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
