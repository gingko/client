import { test, expect, card, setupLifecycleHooks, signedStripeEvent } from './base';
import treeIds from './fixtures/twoTrees.ids.json';
import stripeSuccess from './fixtures/stripeSuccess.json';
import stripeSubscriptionDeleted from './fixtures/stripeSubscriptionDeleted.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Payment', () => {
  test('Has a working Upgrade modal, and lifts the expired-trial lock after payment', async ({ page, login }) => {
    // The app alert()s when you try to edit on an expired trial; Playwright
    // dismisses dialogs by default, so collect them and move on.
    const alerts: string[] = [];
    page.on('dialog', async dialog => {
      alerts.push(dialog.message());
      await dialog.dismiss();
    });

    await login();

    await page.goto(`/${treeIds[0]}`);
    await expect(page).toHaveURL(`/${treeIds[0]}`);
    await expect(page.locator('#app-root')).toBeVisible();
    await expect(page.locator('.spinner')).toHaveCount(0);

    await expect(page.locator('#upgrade-cta')).not.toContainText('Trial Expired');

    await test.step('Expires the trial', async () => {
      const res = await page.request.post('/test/expired');
      expect(res.ok()).toBeTruthy();
      await expect(page.locator('#upgrade-cta')).toContainText('Trial Expired');
    });

    await test.step('Blocks editing while the trial is expired', async () => {
      await page.locator(card(1, 1, 1)).click();
      await page.keyboard.press('Enter');
      await expect.poll(() => alerts).toContain('Trial Expired');
      await expect(page.locator('textarea')).toHaveCount(0);
    });

    await test.step('Shows prices, and persists the model across reopens', async () => {
      await page.locator('#upgrade-button').click();
      await expect(page.locator('#app-root')).toContainText('Upgrade Gingko Writer');

      await expect(page.locator('#currency-selector')).toContainText('USD');
      await expect(page.locator('#upgrade-checkout')).toContainText('$12.75');

      // Closes on the X
      await page.locator('.close-button').click();
      await expect(page.locator('.modal')).toHaveCount(0);

      // Reopening keeps the previously selected currency
      await page.locator('#upgrade-button').click();
      await expect(page.locator('#currency-selector')).toHaveValue('USD');
      await expect(page.locator('#upgrade-checkout')).toContainText('$12.75');
    });

    await test.step('Reacts to billing frequency and currency changes', async () => {
      await page.locator('input#yearly').check();
      await expect(page.locator('#upgrade-checkout')).toContainText('$117');
      await expect(page.locator('#upgrade-checkout')).toContainText('per year');

      await page.locator('#currency-selector').selectOption('INR');
      await expect(page.locator('#upgrade-checkout')).toContainText('₹2400');
    });

    await test.step('Unlocks the trees after a successful checkout webhook', async () => {
      await page.goto('/upgrade/success');

      const hook = await page.request.post('/hooks', signedStripeEvent(stripeSuccess));
      expect(hook.ok()).toBeTruthy();

      await page.locator('.message-cta').click();

      await expect(page).toHaveURL(`/${treeIds[1]}`);
      await expect(page.locator('.spinner')).toHaveCount(0);
      await expect(page.locator('#app-root')).toContainText('Synced');
      await expect(page.locator('#app-root')).toContainText('Another doc, with title');
      await expect(page.locator('#document-header')).not.toContainText('Upgrade');
    });

    await test.step('Editing works again after upgrading', async () => {
      await page.locator(card(1, 1, 1)).click();
      await page.keyboard.press('Enter');
      await expect(page.locator('textarea')).toHaveValue('Another Test doc');
      await page.keyboard.press('Escape');
    });

    await test.step('Locks while the subscription is paused, and unlocks on resume', async () => {
      const updated = (pause_collection: object | null) => ({
        ...stripeSubscriptionDeleted,
        type: 'customer.subscription.updated',
        data: { object: { ...stripeSubscriptionDeleted.data.object, status: 'active', canceled_at: null, ended_at: null, pause_collection } },
      });

      const paused = await page.request.post('/hooks', signedStripeEvent(updated({ behavior: 'void', resumes_at: null })));
      expect(paused.ok()).toBeTruthy();
      await expect(page.locator('#upgrade-cta')).toContainText('Trial Expired');

      const resumed = await page.request.post('/hooks', signedStripeEvent(updated(null)));
      expect(resumed.ok()).toBeTruthy();
      await expect(page.locator('#document-header')).not.toContainText('Upgrade');
    });

    await test.step('Locks again when the subscription is canceled', async () => {
      const hook = await page.request.post('/hooks', signedStripeEvent(stripeSubscriptionDeleted));
      expect(hook.ok()).toBeTruthy();
      await expect(page.locator('#upgrade-cta')).toContainText('Trial Expired');
    });
  });

  test('Rejects webhook events without a valid Stripe signature', async ({ page }) => {
    const unsigned = await page.request.post('/hooks', { data: stripeSuccess });
    expect(unsigned.status()).toBe(400);

    const tampered = signedStripeEvent(stripeSuccess);
    tampered.data = tampered.data.replace('cypress@testing.com', 'attacker@example.com');
    const forged = await page.request.post('/hooks', tampered);
    expect(forged.status()).toBe(400);
  });
});
