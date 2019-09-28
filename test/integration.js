const Application = require('spectron').Application
const {expect} = require('chai')
const electronPath = require('electron') // Require Electron from the binaries included in node_modules.
const path = require('path')
//const robot = require('robotjs')
const { execSync } = require('child_process')
const fs = require('fs')
const { unlink } = require('fs')
const { promisify } = require('util')


const app = new Application({
      path: electronPath,
      args: [path.join(__dirname, "../app")]
    });

describe('Application Start', function () {
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
});
