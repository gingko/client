import { test, expect, setupLifecycleHooks, TEST_EMAIL, TEST_PASSWORD } from './base';

setupLifecycleHooks(test);

// The signup flow needs cypress@testing.com *not* to exist yet, so this spec
// boots against a fixture with no user at all.
test.use({ seed: 'noUser' });

const hasConnectSid = (cookies: { name: string }[]) =>
  cookies.some(c => c.name === 'connect.sid');

test.describe('User Signup Flow', () => {
  test('Signs up, logs out, logs back in, and requests a password reset', async ({ page, context }) => {
    // Eight sequential steps of typing-with-delay (signup, logout, login,
    // forgot-password); the default 15s timeout is marginal on a loaded CI box.
    test.slow();

    await test.step('Clean up any CouchDB user database from a previous run', async () => {
      // `/test/user` drops the CouchDB `userdb-<hex>` that signup created on a
      // previous run. Signup hangs rather than erroring if it still exists, so
      // this must run every time -- including on a CI retry, hence in-test
      // rather than in a beforeAll hook.
      const response = await page.request.delete('/test/user');
      expect(response.ok()).toBeTruthy();
    });

    await test.step('Redirects an anonymous visitor to /signup', async () => {
      await page.goto('/');
      await expect(page).toHaveURL('/signup');
      await expect(page.locator('#signup-email')).toBeFocused();
    });

    await test.step('Shows errors when submitting an empty form', async () => {
      await page.locator('button.cta').click();
      await expect(page.getByText('Please enter an email address.')).toBeVisible();
      await expect(page.getByText('Please enter a password.')).toBeVisible();
    });

    await test.step('Creates a new account', async () => {
      // Uppercase on purpose: the server lowercases the address.
      await page.locator('#signup-email').pressSequentially(TEST_EMAIL.toUpperCase(), { delay: 30 });
      await page.locator('#signup-password').pressSequentially(TEST_PASSWORD, { delay: 30 });
      await page.locator('#email-optin').check();
      await page.locator('button.cta').click();

      // Session established
      await expect(page.locator('button.cta')).toHaveCount(0);
      expect(hasConnectSid(await context.cookies())).toBe(true);
    });

    await test.step('Imports the Welcome Tree', async () => {
      await expect(page).not.toHaveURL(/\/import\/welcome$/);
      await expect(page).toHaveURL(/\/[a-zA-Z0-9]{7}$/);
      await expect(page.locator('#title')).toContainText(/welcome/i);
      await expect(page.locator('#-welcome-to-gingko-writer')).toContainText('Welcome to Gingko Writer');
      await expect(page.locator('#app-root')).toContainText('Welcome to Gingko Writer');
    });

    await test.step('Opens and closes the video tutorials modal', async () => {
      const modal = page.locator('.modal.video-viewer');
      await expect(modal).toBeVisible();
      await page.locator('.close-button').click();
      await expect(modal).not.toBeVisible();
      await expect(page.locator('#app-root')).toContainText('Welcome to Gingko Writer');
    });

    await test.step('Logs out', async () => {
      const logout = page.waitForResponse('**/logout');
      await page.locator('#account-icon').click();
      await expect(page.locator('#account-menu')).toBeVisible();
      await page.locator('#logout-button').click();
      await logout;

      expect(hasConnectSid(await context.cookies())).toBe(false);
      expect(await page.evaluate(() => localStorage.getItem('gingko-session-storage'))).toBeNull();
      await expect(page).toHaveURL('/login');
      await expect(page.locator('button.cta')).toContainText('Login');
    });

    await test.step('Logs back in', async () => {
      await page.goto('/');
      await expect(page).toHaveURL('/signup');

      await page.getByRole('link', { name: 'Login' }).click();
      await expect(page).toHaveURL('/login');

      await page.locator('#email-input').pressSequentially(TEST_EMAIL, { delay: 30 });
      await page.locator('#password-input').pressSequentially(TEST_PASSWORD, { delay: 30 });
      await page.locator('button.cta').click();

      await expect(page).toHaveURL(/\/[a-zA-Z0-9]{7}$/);
      await expect(page.locator('#app-root')).toContainText('Welcome to Gingko Writer');
      await expect(page.locator('#email-confirm-banner')).toHaveCount(0);
      await expect(page.locator('button.cta')).toHaveCount(0);
      expect(hasConnectSid(await context.cookies())).toBe(true);
    });

    await test.step('Redirects to /login once the session cookie is gone', async () => {
      await context.clearCookies();
      await page.goto('/');
      await expect(page).toHaveURL('/login');
    });

    await test.step('Sends a password reset email from Forgot Password', async () => {
      await page.goto('/login');

      const forgot = page.locator('a.forgot-password');
      await expect(forgot).toHaveAttribute('href', '/forgot-password');
      await forgot.click();
      await expect(page).toHaveURL(/\/forgot-password/);

      // The form renders before the Elm runtime wires up its input handlers, so
      // a keystroke fired too early leaves `model.email` blank and the submit
      // silently fails validation. Retry until the value actually sticks.
      const emailInput = page.locator('input[type=email]');
      await expect(async () => {
        await emailInput.fill('');
        await emailInput.pressSequentially(TEST_EMAIL, { delay: 20 });
        expect(await emailInput.inputValue()).toBe(TEST_EMAIL);
      }).toPass({ timeout: 7000 });

      await page.locator('button.cta').click();

      await expect(page.getByText('Reset Email Sent')).toBeVisible();
    });
  });
});
