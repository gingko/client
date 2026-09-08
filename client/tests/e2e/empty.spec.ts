import { test, expect, setupLifecycleHooks } from './base';

setupLifecycleHooks(test);

// A user with no documents at all.
test.use({ seed: 'empty' });

test.describe('Empty State', () => {
  test('Shows the empty page, creates a tree, and returns to empty on deletion', async ({ page, login }) => {
    // Deleting a tree pops a confirm(); Playwright dismisses dialogs by default,
    // which would silently cancel it.
    page.on('dialog', dialog => dialog.accept());

    await login();

    await page.goto('/');
    await expect(page).toHaveURL('/');
    await expect(page.locator('#empty-message')).toContainText("You don't have any documents");

    await test.step('Goes to a newly created tree', async () => {
      await page.locator('#new-button').click();
      await page.locator('#templates-block #template-new').click();
      await expect(page).toHaveURL(/\/[a-zA-Z0-9]{7}$/);
    });

    await test.step('Goes back to the empty page once the last tree is deleted', async () => {
      await page.locator('#documents-icon').click();

      const firstDoc = page.locator('#sidebar-document-list-wrap .sidebar-document-item').first();
      await firstDoc.click({ button: 'right' });
      await page.locator('#sidebar-context-menu').getByText('Delete Tree').click();

      await expect(page).toHaveURL('/');
      await expect(page.locator('#no-documents')).toBeVisible();
      await expect(page.locator('#empty-message')).toContainText("You don't have any documents");
    });
  });
});
