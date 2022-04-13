import { _electron as electron, test, expect, ElectronApplication, Page, BrowserContext } from "@playwright/test";
const path = require('path')
import * as fs from 'fs/promises';
import * as fsStd from 'fs';

let electronApp: ElectronApplication;
let homeWindow: Page;
let newDocWindow : Page;
let context: BrowserContext;


test.beforeAll( async () => {
  electronApp = await electron.launch({ args: ["./app"] });
  context = electronApp.context();
  homeWindow = await electronApp.firstWindow();
})


test.describe('Check Home Page', async () => {
  test('Check Title', async () => {
    const title = await homeWindow.title();
    expect(title).toBe("Gingko Writer - Home");
  })

  test('Opens New Document', async () => {
    let [window] = await Promise.all([electronApp.waitForEvent('window'), homeWindow.click("#new-doc-button")]);
    newDocWindow = window;
    expect(newDocWindow).toBeTruthy();
    await expect(newDocWindow.locator('#app-root textarea')).toBeTruthy();
  })

  test('Can type in box and save', async  () => {
    const textarea = await newDocWindow.locator('textarea.edit');
    await textarea.focus()
    await newDocWindow.keyboard.type('Hi!');
    await expect(textarea).toHaveValue('Hi!')
    await newDocWindow.keyboard.press('Control+Enter');

    const savedCardView = await newDocWindow.locator('.card.active .view');
    await expect(savedCardView).toHaveText('Hi!');
  })

  test('Saves to file properly', async () => {
    const dir = '/tmp';
    const files = await fs.readdir(dir);
    const latestFile =
      files
        .filter(async (file) => (await fs.stat(path.join(dir, file))).isFile())
        .map((file) => {
          let stats = fsStd.statSync(path.join(dir, file));
          return {file: file, mtime: stats.mtime};
        })
        .sort((a, b) => b.mtime.getTime() - a.mtime.getTime())
        .map(x => x.file)[0];
    const fileContents = await fs.readFile(path.join(dir, latestFile), {encoding: "utf8"});
    expect(fileContents).toEqual('<gingko-card id="1">\n\nHi!\n\n</gingko-card>');
  })

  test('Splits card correctly on Control+J', async () => {
    // Enter for edit mode
    await newDocWindow.keyboard.press('Enter');
    const rootCardTextarea = await newDocWindow.locator('textarea.edit');
    await expect(rootCardTextarea).toBeFocused();

    // Add '\nSomething'
    await newDocWindow.keyboard.press('Enter');
    await newDocWindow.keyboard.type('Something');

    // Split down
    await newDocWindow.keyboard.press('Control+J');
    const rootCard = getCard(newDocWindow, 1,1,1);
    await expect(rootCard).toHaveText('Hi!\nSomething',{useInnerText: true});

    // Add text to newly split card
    await expect(rootCardTextarea).toBeFocused();
    await newDocWindow.keyboard.type('xyzuvw');
    await newDocWindow.keyboard.press('ArrowLeft');
    await newDocWindow.keyboard.press('ArrowLeft');
    await newDocWindow.keyboard.press('ArrowLeft');
    await newDocWindow.keyboard.press('Control+J');

    const xyzCard = getCard(newDocWindow, 1, 1, 2);
    const uvwTextarea = getCard(newDocWindow, 1, 1, 3).locator('textarea.edit');
    await expect(xyzCard).toHaveText('xyz');
    await expect(uvwTextarea).toHaveValue('uvw');

    // Check that cursor positon is set correctly
    expect(await newDocWindow.evaluate(async () => {
      await new Promise(r => setTimeout(r, 200));
      // @ts-ignore
      return window.elmMessages.map(m => m.tag).filter(t => t == "SetCursorPosition")
    }))
      .toHaveLength(1);

    // @ts-ignore
    //expect(await uvwTextarea.evaluate(node => [node.selectionStart, node.selectionEnd])).toEqual([0,0]);
  })

  test('Saves to file on Control+S', async () => {
    newDocWindow.on('console', console.log);
    let filePath = path.join(__dirname, 'test-here.gkw');
    await electronApp.evaluate((process, pathArg)=>{
      // @ts-ignore
      process.dialog.showSaveDialog = () => Promise.resolve({canceled: false, filePath: pathArg});
    }, filePath);

    await newDocWindow.keyboard.press('Control+Enter');
    await newDocWindow.keyboard.press('Control+S');
    expect(newDocWindow.locator('div.view'))

    const title = await electronApp.evaluate((process) => {
      const mainWindow = process.BrowserWindow.getAllWindows()[0];
      return mainWindow.title
      return new Promise((resolve)=> resolve(mainWindow.title));
    })
    expect(title).toEqual("test-here.gkw - Gingko Writer");
    expect(await fs.stat(filePath)).toHaveProperty('ctimeMs');
  })
})


test.afterAll( async () => {
  await electronApp.close();
  await fs.unlink(path.join(__dirname, 'test-here.gkw'));
})


/* ==== Helpers ==== */

function getCard(rootEl : Page, colNum : number, groupNum : number, cardNum : number) {
  return rootEl.locator(`#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1}) > .card:nth-child(${cardNum})`);
}
/*
Cypress.Commands.add('getColumn', (colNum) => {
  cy.get(`#column-container > .column:nth-child(${colNum})`)
})


Cypress.Commands.add('getGroup', (colNum, groupNum) => {
  cy.get(`#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1})`)
})
 */