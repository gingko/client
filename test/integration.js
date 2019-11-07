const Application = require("spectron").Application;
const {expect} = require("chai");
const electronPath = require("electron");
const path = require("path");
const fs = require("fs-extra");
//const ks = require("node-key-sender");
const { keyboard, Key } = require("@nut-tree/nut-js");


const commandOrControl = process.platform == "darwin" ? "\uE03D" : "Control";
const commandOrControlKey = process.platform == "darwin" ? Key.LeftSuper : Key.LeftControl;


describe("Application Start", function () {
  const app = new Application({
        path: electronPath,
        args: [path.join(__dirname, "../app")]
      });

  this.timeout(10000);

  beforeEach(() => {
    return app.start();
  });

  afterEach(function () {
    if (app && app.isRunning()) {
      return app.stop();
    }
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


describe("Actions on Untitled Document", function () {
  const savePath = path.join(__dirname, "test-save-1.gko");

  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: "1"
          , DIALOG_SAVE_PATH: savePath
          },
        args: [path.join(__dirname, "../app"), "new"]
      });

  this.timeout(10000);

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
    await keyboard.type(Key.Enter);
    const textareaExists = await app.client.waitForExist("#card-edit-1", 800);
    expect(textareaExists).to.be.true;
  });

  it("should have text \"Hello World\" in card after typing it", async () => {
    await app.client.keys(["Hello World"]);
    const textareaValue = await app.client.getValue("#card-edit-1");
    expect(textareaValue).to.equal("Hello World");
  });

  it("should say \"Unsaved Changes...\" in save indicator", async () => {
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Unsaved Changes...");
  });

  it("should have title \"*Untitled - Gingko\"", async () => {
    let windowTitle = await app.browserWindow.getTitle();
    expect(windowTitle).to.equal("*Untitled - Gingko");
  });

  it(`should switch to navigation mode when pressing ${commandOrControl}+Enter`, async () => {
    // Modifier keys are sticky, so they need to be triggered again to release.
    await app.client.keys([commandOrControl, "Enter", commandOrControl]);
    const cardViewExists = await app.client.waitForExist("#card-1 .view", 800);
    expect(cardViewExists).to.be.true;
  });

  it("should show \"Hello World\" in card view", async () => {
    const cardText = await app.client.getText("#card-1 .view");
    expect(cardText).to.equal("Hello World");
  });

  it("should eventually say \"Backup Saved\" in save indicator", async () => {
    await app.client.pause(4000);
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Backup Saved");
  });

  it(`should create a new card on ${commandOrControl}+Right`, async () => {
    // Modifier keys are sticky, so they need to be triggered again to release.
    await app.client.keys([commandOrControl, "ArrowRight", commandOrControl]);
    await app.client.keys("A child card");
    const cardViewExists = await app.client.waitForExist("[id^=card-node-]", 800);
    expect(cardViewExists).to.be.true;
  });

  it(`should save on ${commandOrControl}+S`, async () => {
    await keyboard.type(commandOrControlKey, Key.S);
    await app.client.pause(1000);
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Saved");
  });

  it("should have saved to file", async () => {
    expect(fs.existsSync(savePath)).to.be.true;
  });

  it("should say \"Unsaved Changes...\" on new changes", async () => {
    await app.client.keys("\nWith some changes after saving");
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Unsaved Changes...");
  });

  it("should eventually say \"Saved\" in save indicator when saving card", async () => {
    // Modifier keys are sticky, so they need to be triggered again to release.
    await app.client.keys([commandOrControl, "Enter", commandOrControl]);
    await app.client.pause(500);
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Saved");
  });

  it("should enter edit mode on clicking edit button", async () => {
    app.client.$("span.card-btn.edit").click();
    await app.client.pause(500);
    await app.client.keys("\nThis should be saved from edit mode even on exit");
    const textareaValue = await app.client.getValue("[id^=card-edit-node-]");
    expect(textareaValue).to.equal(
      "A child card\n" +
      "With some changes after saving\n" +
      "This should be saved from edit mode even on exit"
    );
  });
});


describe("Verify Data Saved from Untitled", function () {
  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: "1"
          , DIALOG_CHOICE: "0" // Close Without Saving
          },
        args: [path.join(__dirname, "../app"), path.join(__dirname, "test-save-1.gko")]
      });

  this.timeout(10000);

  before(() => {
    return app.start();
  });

  after(function () {
    if (app && app.isRunning()) {
      return app.stop();
    }
  });

  it("should have loaded the cards and content", async () => {
    await app.client.waitUntilWindowLoaded();

    let cardContent = [];
    cardContent[0] = await app.client.$("#card-1").getText();
    cardContent[1] = await app.client.$("[id^=card-node-]").getText();
    expect(cardContent).to.eql([
      "Hello World",
      "A child card\n" +
        "With some changes after saving\n"+
        "This should be saved from edit mode even on exit"
    ]);
  });
});


describe("Actions on Loaded Document", function () {
  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: "1"
          , DIALOG_CHOICE: "0" // Close Without Saving
          },
        args: [path.join(__dirname, "../app"), path.join(__dirname, "test-1.gko")]
      });

  this.timeout(10000);

  before(async () => {
    await fs.copy(path.join(__dirname, "source-test-1.gko"), path.join(__dirname, "test-1.gko"));
    return app.start();
  });

  after(() => {
    if (app && app.isRunning()) {
      return app.stop();
    }
  });

  it("should have loaded the cards and content", async () => {
    await app.client.waitUntilWindowLoaded();

    let cardContent = [];
    cardContent[0] = await app.client.$("#card-1").getText();
    cardContent[1] = await app.client.$("#card-node-2128918683").getText();
    cardContent[2] = await app.client.$("#card-node-1367859142").getText();
    cardContent[3] = await app.client.$("#card-node-1713013080").getText();
    cardContent[4] = await app.client.$("#card-node-1829485524").getText();

    expect(cardContent).to.eql([
      "This is the root card.",
      "The first child is here.",
      "Second child.",
      "A third one.",
      "And second child's child. A grandchild!",
    ]);
  });

  it("should say \"Saved\" in save indicator", async () => {
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Saved");
  });
});


describe("Importing JSON Document", function () {
  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: "1"
          , DIALOG_CHOICE: "0" // Close Without Saving
          , DIALOG_OPEN_PATH: path.join(__dirname, "test-1.json")
          },
        args: [path.join(__dirname, "../app")]
      });

  this.timeout(10000);

  before(async () => {
    await fs.copy(path.join(__dirname, "source-test-1.json"), path.join(__dirname, "test-1.json"));
    return app.start();
  });

  after(() => {
    if (app && app.isRunning()) {
      return app.stop();
    }
  });

  it("should create a document window on selecting file from 'Import JSON'", async () => {
    await app.client.waitUntilWindowLoaded();
    await app.client.click("#template-import");
    await app.client.pause(600);
    await app.client.windowByIndex(0);
    const count = await app.client.getWindowCount();
    const title = await app.browserWindow.getTitle();
    expect(count).to.eq(1);
    return expect(title).to.eq("Untitled - Gingko");
  });

  it("should have loaded the cards and content", async () => {
    await app.client.waitUntilWindowLoaded();

    let cardContent = [];
    cardContent[0] = await app.client.$("#card-1").getText();
    cardContent[1] = await app.client.$("#card-2").getText();
    cardContent[2] = await app.client.$("#card-3").getText();
    cardContent[3] = await app.client.$("#card-5").getText();
    cardContent[4] = await app.client.$("#card-4").getText();

    expect(cardContent).to.eql([
      "This is the root card.",
      "The first child is here.",
      "Second child.",
      "A third one.",
      "And second child's child. A grandchild!",
    ]);
  });

  it("should say \"Backup Saved\" in save indicator", async () => {
    const saveIndicatorText = await app.client.getText("#save-indicator span");
    expect(saveIndicatorText).to.equal("Backup Saved");
  });
});


describe("License Window", function () {
  const app = new Application({
        path: electronPath,
        env:
          { RUNNING_IN_SPECTRON: "1"
          , DIALOG_CHOICE: "0" // Close Without Saving
          , MENU_ITEM_ID: "enterLicense"
          , MENU_ITEM_ACCELERATOR: "Alt+CommandOrControl+Shift+y"
          },
        args: [path.join(__dirname, "../app"), "new"]
      });

  this.timeout(10000);

  before(() => {
    return app.start();
  });

  after(function () {
    if (app && app.isRunning()) {
      return app.stop();
    }
  });

  it("should show the license window", async () => {
    await app.client.waitUntilWindowLoaded();
    await keyboard.type(Key.LeftAlt, commandOrControlKey, Key.LeftShift, Key.Y);
    await app.client.windowByIndex(1);
    const title = await app.browserWindow.getTitle();
    expect(title).to.equal("Register Gingko");
  });
});


after(async () => {
  let testGkoPath = path.join(__dirname, "test-1.gko");
  let testJSONPath = path.join(__dirname, "test-1.json");
  let testSavePath = path.join(__dirname, "test-save-1.gko");

  if (fs.existsSync(testGkoPath)){
    await fs.unlink(testGkoPath);
  }

  if (fs.existsSync(testJSONPath)){
    await fs.unlink(testJSONPath);
  }

  if (fs.existsSync(testSavePath)){
    await fs.unlink(testSavePath);
  }
});
