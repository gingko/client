import { test, expect, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Loading a document', () => {
  // Every Playwright test starts from a cold browser profile, which is exactly
  // the situation a real user is in on a new device, in a private window, or
  // after clearing site data. Nothing is in Dexie yet, so this only passes
  // because LoadDocument waits for the first `trees` sync before it will
  // conclude a document is missing.
  //
  // treeIds[0] is deliberately not the tree `/` redirects to, so this only
  // passes if the document was loaded from the URL.
  test('Loads a document opened directly by URL', async ({ page, login }) => {
    await login();

    await page.goto(`/${treeIds[0]}`);

    await expect(page.locator('#app-root')).toContainText('Hello Test doc');
    await expect(page).toHaveURL(`/${treeIds[0]}`);
  });

  test('Never flashes the "empty" state for a user who has documents', async ({ page, login }) => {
    await login();

    await page.goto('/');
    await expect(page).toHaveURL(`/${treeIds[1]}`);

    // The empty-document placeholder sends `EmptyMessageShown` when it renders.
    // A user with trees must never see it, even for a frame while Dexie and the
    // websocket sync are still catching up.
    const tags = await page.evaluate(() => (window as any).elmMessages.map((m: any) => m.tag));
    expect(tags).not.toContain('EmptyMessageShown');

    // A logged-in visitor to /login is bounced to the root, i.e. the first tree.
    await page.goto('/login');
    await expect(page).toHaveURL(`/${treeIds[1]}`);
  });
});
