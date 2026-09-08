import { test, expect, setupLifecycleHooks, waitForStableFocus } from './base';

setupLifecycleHooks(test);

test.use({ seed: 'empty' });

test.describe('Offline', () => {
  // Ported straight from `describe.skip` in the Cypress suite. The network
  // interception it needs (`cy.intercept('/db*', { forceNetworkError: true })`)
  // was never wired up -- the block is commented out in the original -- so
  // without it "Saved Offline" never appears and the test can't pass. Kept
  // skipped, and as a home for a real offline test once one exists.
  test.skip('Can save offline changes', async ({ page, login }) => {
    await login();

    await page.goto('/new');
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/);
    await expect(page.locator('#app-root')).toBeVisible();
    await expect(page.locator('.spinner')).toHaveCount(0);

    const editor = page.locator('textarea');
    await editor.pressSequentially('Hello Test doc', { delay: 30 });
    await page.keyboard.press('Control+l');
    await waitForStableFocus(page);
    await editor.pressSequentially('Child card', { delay: 30 });
    await page.keyboard.press('Control+j');
    await waitForStableFocus(page);
    await editor.pressSequentially('Another Child card', { delay: 30 });
    await page.keyboard.press('Control+Enter');
    await expect(page.locator('#save-indicator')).toContainText('Synced');

    // TODO: actually take the connection offline here.

    await page.keyboard.press('Enter');
    await editor.pressSequentially('\nSome offline changes', { delay: 30 });
    await page.keyboard.press('Control+Enter');
    await expect(page.locator('#save-indicator')).toContainText('Saved Offline');
  });
});
