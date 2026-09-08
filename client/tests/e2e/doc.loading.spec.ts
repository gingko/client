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

  test('Redirects a logged-in visitor from /login to their first tree', async ({ page, login }) => {
    await login();

    await page.goto('/');
    await expect(page).toHaveURL(`/${treeIds[1]}`);

    await page.goto('/login');
    await expect(page).toHaveURL(`/${treeIds[1]}`);
  });

  // Migrated from the Cypress `doc.loading` spec's "Should not show 'Empty'
  // message" assertion. It passed there only because `cy.signup_with` had
  // already visited the app in setup, priming the local document list before
  // the test navigated. On a cold profile the `Empty` view renders for a frame
  // while the first `trees` sync is still in flight, and fires
  // `EmptyMessageShown` -- the same class of premature "nothing here" bug that
  // `db13b10b` fixed for direct-URL document loading (see the note on the test
  // above). Left failing on purpose until the empty state waits for that sync.
  test.fixme('Never flashes the "empty" state on a cold profile for a user who has documents', async ({ page, login }) => {
    await login();

    await page.goto('/');
    await expect(page).toHaveURL(`/${treeIds[1]}`);

    const tags = await page.evaluate(() => (window as any).elmMessages.map((m: any) => m.tag));
    expect(tags).not.toContain('EmptyMessageShown');
  });
});
