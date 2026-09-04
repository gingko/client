import { test, expect } from '@playwright/test';
import config from '../../config.js';
import { setupLifecycleHooks } from './shared';

setupLifecycleHooks(test);

test.use({ storageState: `${process.cwd()}/tests/e2e/.auth/user.json` });

test.describe('Document UI', () => {
  const testEmail = 'cypress@testing.com';

  test('Has working header menus and shortcut help', async ({ page }) => {
    const emailText = 'Contact Support';

    // Visit a new document
    await page.goto(config.TEST_SERVER + '/new');

    // Check URL pattern matches document ID
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{7}$/);

    // Wait for sync
    await expect(page.locator('text=Synced')).toBeVisible();

    // Check that email support text is not visible initially
    await expect(page.locator('#app-root')).not.toContainText(emailText);

    // Click help icon
    await page.locator('#help-icon').click();

    // Check help modal appears with expected content
    await expect(page.locator('.modal.help-modal')).toContainText(emailText);
    await expect(page.locator('.modal.help-modal')).toContainText('FAQ');

    // Click email support link
    await page.locator('#email-support').click();

    // Check contact form is visible and has correct values
    await expect(page.locator('#contact-form')).toBeVisible();
    await expect(page.locator('#contact-from-email')).toHaveValue(testEmail);
    await expect(page.locator('#contact-subject')).toHaveValue('Could you help me with this?');

    // Check body field has focus
    await expect(page.locator('#contact-body')).toBeFocused();

    // Type in the contact body
    await page.locator('#contact-body').pressSequentially('doc.ui', { delay: 30 });

    // Intercept the contact form submission
    await page.route('/pleasenospam', async route => {
      await route.fulfill({ status: 200, body: '' });
    });

    // Submit the form
    await page.locator('#contact-send').click();

    // Wait for form to disappear
    await expect(page.locator('#contact-form')).not.toBeVisible();

    // Test sidebar toggle via brand icon
    await expect(page.locator('#sidebar-document-list-wrap')).not.toBeVisible();
    await page.locator('#brand').click();
    await expect(page.locator('#sidebar-document-list-wrap')).toBeVisible();
    await page.locator('#brand').click();
    await expect(page.locator('#sidebar-document-list-wrap')).not.toBeVisible();

    // Test shortcuts tray toggle
    await expect(page.locator('#app-root')).not.toContainText('Keyboard Shortcuts');
    await page.locator('#shortcuts-tray').click();
    await expect(page.getByRole('heading', { name: 'Keyboard Shortcuts' })).toBeVisible();

    // Check for Edit Mode indicator
    await expect(page.locator('text=(Edit Mode)')).toBeVisible();

    // Write in card and switch mode
    await page.locator('textarea').pressSequentially('This is a test', { delay: 30 });

    // Save with Ctrl+Enter
    await page.keyboard.press('Control+Enter');

    // Edit Mode indicator should be gone
    await expect(page.locator('#app-root')).not.toContainText('(Edit Mode)');

    // Open edit mode again and test external link
    await page.keyboard.press('Enter');
    const shortcutsLink = page.locator('#shortcuts a');
    await expect(shortcutsLink).toHaveAttribute('target', '_blank');
    await page.keyboard.press('Escape');

    // Test Word Count modal
    await page.locator('#doc-settings-icon').click();
    await page.locator('#wordcount-menu-item').click();

    await expect(page.locator('.modal-header h2')).toContainText('Word & Character Counts');
    await expect(page.locator('text=Total : 4 words')).toBeVisible();

    await page.keyboard.press('Escape');

    // Test mobile buttons visibility
    await expect(page.locator('#mobile-buttons')).not.toBeVisible();

    // Switch to mobile viewport
    await page.setViewportSize({ width: 360, height: 640 });
    await expect(page.locator('#mobile-buttons')).toBeVisible();

    // Test mobile edit button
    await page.locator('#mbtn-edit').click();
    const textarea = page.locator('textarea');
    await expect(textarea).toBeVisible();
    await expect(textarea).toBeFocused();

    // Write additional text
    await textarea.press('Enter');
    await textarea.pressSequentially('here', { delay: 30 });

    // Save with mobile button
    await page.locator('#mbtn-save').click();
    await expect(page.locator('.view')).toContainText('here');

    // Test "add child" button
    await page.locator('#mbtn-add-right').click();
    await expect(textarea).toHaveValue(/.*/);  // Wait for textarea value to be set
    await textarea.pressSequentially('axc', { delay: 30 });
    await page.locator('#mbtn-save').click();

    const column2 = page.locator('#column-container > .column:nth-child(2) .view');
    await expect(column2).toContainText('axc');

    // Test "add below" button
    await page.locator('#mbtn-add-down').click();
    await expect(textarea).toHaveValue(/.*/);  // Wait for textarea value to be set
    await textarea.pressSequentially('sdf', { delay: 30 });
    await page.locator('#mbtn-save').click();

    const card_2_1_2 = page.locator('#column-container > .column:nth-child(2) > .group:nth-child(2) > .card:nth-child(2) .view');
    await expect(card_2_1_2).toContainText('sdf');

    // Test "add above" button
    await page.locator('#mbtn-add-up').click();
    await expect(textarea).toHaveValue(/.*/);  // Wait for textarea value to be set
    await textarea.pressSequentially('lak', { delay: 30 });
    await page.locator('#mbtn-save').click();

    const card_2_1_2_after_insert = page.locator('#column-container > .column:nth-child(2) > .group:nth-child(2) > .card:nth-child(2) .view');
    await expect(card_2_1_2_after_insert).toContainText('lak');

    // Test "nav up" button
    await page.locator('#mbtn-nav-up').click();
    await expect(page.locator('.card.active')).toContainText('axc');

    // Test "nav down" button (twice)
    await page.locator('#mbtn-nav-down').click();
    await page.locator('#mbtn-nav-down').click();
    await expect(page.locator('.card.active')).toContainText('sdf');

    // Test "nav left" button
    await page.locator('#mbtn-nav-left').click();
    await expect(page.locator('.card.active')).toContainText('This is a test');

    // Test "nav right" button
    await page.locator('#mbtn-nav-right').click();
    await expect(page.locator('.card.active')).toContainText('sdf');

    // Test "cancel" button
    await page.locator('#mbtn-edit').click();
    await page.locator('#mbtn-cancel').click();
    await expect(textarea).not.toBeVisible();
  });
});
