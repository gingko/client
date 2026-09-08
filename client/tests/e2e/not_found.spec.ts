import { test, expect, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Not Found (logged-in user)', () => {
  test('Redirects an unknown document to its 404 page, then back to real trees', async ({ page, login }) => {
    await login();

    await page.goto('/aaaaa');
    await expect(page).toHaveURL('/aaaaa/404-not-found');
    await expect(page.locator('#app-root')).toContainText("Hmm, we couldn't find this document");

    // The other documents are still reachable
    await page.goto(`/${treeIds[0]}`);
    await expect(page.locator('#app-root')).toContainText('Hello Test doc');

    await page.goto(`/${treeIds[1]}`);
    await expect(page.locator('#app-root')).toContainText('Another Test doc');
  });
});
