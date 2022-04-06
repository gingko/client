import { _electron as electron, test, expect, ElectronApplication, Page, BrowserContext } from "@playwright/test";

let electronApp: ElectronApplication;
let homeWindow: Page;
let newDocWindow : Page;
let context: BrowserContext;


test.beforeAll( async () => {
  electronApp = await electron.launch({ args: ["./app"] });
  context = electronApp.context();
  await context.tracing.start({ screenshots: true, snapshots: true });
  homeWindow = await electronApp.firstWindow();

  await homeWindow.screenshot({ path: 'tests/screenshot/homeWindow.png' });
  expect(await homeWindow.screenshot()).toMatchSnapshot('homeWindow.png');
})


test.describe('Check Home Page', async () => {
  test('Check Title', async () => {
    const title = await homeWindow.title();
    expect(title).toBe("Gingko Writer - Home");
  })

  test('Opens New Document', async () => {
    await homeWindow.click("#new-doc-button");
    newDocWindow = await electronApp.windows()[0]
    expect(newDocWindow).toBeTruthy();
    await expect(newDocWindow).toHaveTitle("Gingko Writer Desktop");
  })
})


test.afterAll( async () => {
  await context.tracing.stop({ path: 'tests/tracing/trace.zip' });
  await electronApp.close();
})