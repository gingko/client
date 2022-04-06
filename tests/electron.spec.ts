import { _electron as electron, test, expect, ElectronApplication, Page, BrowserContext } from "@playwright/test";


test("Launch electron app", async () => {
  const electronApp = await electron.launch({ args: ["./app"] });

  const windowState: {
    isVisible: boolean;
    isDevToolsOpened: boolean;
    isCrashed: boolean;
  } = await electronApp.evaluate(async ({ BrowserWindow }) => {
    const mainWindow = BrowserWindow.getAllWindows()[0];

    const getState = () => ({
      isVisible: mainWindow.isVisible(),
      isDevToolsOpened: mainWindow.webContents.isDevToolsOpened(),
      isCrashed: mainWindow.webContents.isCrashed(),
    });

    return new Promise((resolve) => {
      if (mainWindow.isVisible()) {
        resolve(getState());
      } else {
        mainWindow.once("ready-to-show", () =>
          setTimeout(() => resolve(getState()), 0)
        );
      }
    });
  });

  expect(windowState.isVisible).toBeTruthy();
  expect(windowState.isDevToolsOpened).toBeFalsy();
  expect(windowState.isCrashed).toBeFalsy();

  await electronApp.close();
});


test.describe('Check Home Page', async () => {
  let electronApp: ElectronApplication;
  let firstWindow: Page;
  let context: BrowserContext;


  test.beforeAll( async () => {
    electronApp = await electron.launch({ args: ["./app"] });
    context = electronApp.context();
    await context.tracing.start({ screenshots: true, snapshots: true });
    firstWindow = await electronApp.firstWindow();

    await firstWindow.screenshot({ path: 'tests/screenshot/firstWindow.png' });
    expect(await firstWindow.screenshot()).toMatchSnapshot('firstWindow.png');
  })

  test('Check Title', async () => {
    const title = await firstWindow.title();
    expect(title).toBe("Gingko Writer - Home");
  })
})