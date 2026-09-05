import { test, expect, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Loading a document', () => {
  // Every Playwright test starts from a cold browser profile, which is exactly
  // the situation a real user is in on a new device, in a private window, or
  // after clearing site data. `LoadDocument` in src/shared/doc.js decides
  // NotFound off an empty Dexie, before the websocket has delivered the
  // document list -- and the app then rewrites the URL to
  // `/<treeId>/404-not-found`, so a refresh doesn't recover: the URL is now
  // itself the 404 route. Only a second visit to the original URL works, by
  // which point Dexie is warm.
  test.fail();

  // treeIds[0] is deliberately not the tree `/` redirects to, so this only
  // passes if the document was loaded from the URL.
  test('Loads a document opened directly by URL', async ({ page, login }) => {
    await login();

    await page.goto(`/${treeIds[0]}`);

    await expect(page.locator('#app-root')).toContainText('Hello Test doc');
    await expect(page).toHaveURL(`/${treeIds[0]}`);
  });
});
