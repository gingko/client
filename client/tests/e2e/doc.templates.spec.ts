import { test, expect, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

// The Cypress version signed a brand-new user up and checked the Welcome Tree
// and its video-tutorials modal too. Both of those are already covered by
// auth.spec.ts, so this keeps only the part unique to it -- the template
// selector -- and reaches it from an ordinary logged-in session rather than a
// second signup flow (which would race auth.spec.ts on the shared CouchDB).
test.describe('Template Selector', () => {
  test('Offers every template, and keeps them reachable on a narrow screen', async ({ page, login }) => {
    await login();

    await page.goto('/');
    await expect(page).toHaveURL(`/${treeIds[1]}`);
    await expect(page.locator('.spinner')).toHaveCount(0);

    // "New" opens the template selector
    await page.locator('#new-icon').click();
    await expect(page.locator('#templates-block')).toBeVisible();

    await expect(page.locator('#template-new')).toBeVisible();
    await expect(page.locator('#template-import')).toBeVisible();
    await page.locator('#template-timeline').scrollIntoViewIfNeeded();
    await expect(page.locator('#template-timeline')).toBeVisible();

    // Every template is still reachable once the viewport is phone-sized
    await page.setViewportSize({ width: 600, height: 900 });

    await page.locator('#template-new').scrollIntoViewIfNeeded();
    await expect(page.locator('#template-new')).toBeVisible();

    await page.locator('#template-import-text').scrollIntoViewIfNeeded();
    await expect(page.locator('#template-import-text')).toBeVisible();

    await page.locator('#template-import').scrollIntoViewIfNeeded();
    await expect(page.locator('#template-import')).toBeVisible();
  });
});
