import {test, expect} from "@playwright/test";
import { card, seedWith, setupLifecycleHooks } from "./shared";
import config from "../../config";
import treeIds from "../../cypress/fixtures/twoTrees.ids.json";

setupLifecycleHooks(test)

test.beforeAll(async () => {
  seedWith('twoTrees');
})

test.use({storageState: `${process.cwd()}/tests/e2e/.auth/user.json`});

test('Can perform basic actions on New tree', async ({page}) => {
  await page.goto(`${config.TEST_SERVER}/`);

  // Should go to first doc
  await expect(page).toHaveURL(`${config.TEST_SERVER}/${treeIds[1]}`);

  const firstCard = page.locator('text=Another Child card');
  expect(firstCard).toBeVisible();

  await firstCard.click();

  // Enters and exits fullscreen mode on clicking
  await page.keyboard.press('Shift+Enter');
  await expect(page.locator('#app-fullscreen')).toBeVisible();

  //  cy.get('#fullscreen-main').should('be.visible')
  await expect(page.locator('#fullscreen-main')).toBeVisible();

  //  cy.get('#fullscreen-exit').click()
  await expect(page.locator('#fullscreen-exit')).toBeVisible();
  await page.locator('#fullscreen-exit').click();

  await expect(page.locator('#app-fullscreen')).not.toBeVisible();
  await expect(page.locator('#fullscreen-main')).not.toBeVisible();
  await expect(page.locator('textarea.edit')).toHaveValue('# 3\nAnother Child card');
  await page.keyboard.press('Escape');

  // Toggles fullscreen mode with Shift+Enter
  await page.keyboard.press('Shift+Enter');
  await expect(page.locator('#app-fullscreen')).toBeVisible();
  await expect(page.locator('#fullscreen-main')).toBeVisible();
  await expect(page.locator('textarea').nth(1)).toBeFocused();

  // Expect all textareas to have the attribute 'data-private=lipsum'
  for (const textarea of await page.locator('textarea').all()) {
    expect(await textarea.getAttribute('data-private')).toBe('lipsum');
  }

  // Test typing
  const focused = page.locator(':focus');
  await focused.pressSequentially(' test', { delay: 50 });
  await expect(page.locator('#save-indicator')).toContainText('Synced');
  await expect(focused).toHaveValue('# 3\nAnother Child card test');


  // Test change card focus
  const firstTextarea = page.locator('textarea').first();
  await firstTextarea.click();
  await firstTextarea.press('Enter');
  await firstTextarea.pressSequentially('abc', { delay: 50 });
  await expect(page.locator('#fullscreen-buttons #save-indicator')).toContainText('Synced');
  await page.keyboard.press('Escape');
  await expect(page.locator('#app-fullscreen')).not.toBeVisible();
  await expect(page.locator('#fullscreen-main')).not.toBeVisible();
  await expect(page.locator('textarea')).not.toBeVisible();
  await expect(page.locator(card(2,1,1))).toContainText('cardabc');
  await expect(page.locator(card(2,1,2))).toContainText('card test');

  // Make sure cardabc doesn't have newline at end
  await page.locator(card(2,1,1)).click();
  await page.locator('.edit').click();
  await expect(page.locator('textarea')).toHaveValue('# 2\nChild card\nabc');
  await page.keyboard.press('Escape');

  // Field preserved when exiting fullscreen
  await page.keyboard.press('Shift+Enter');
  await focused.pressSequentially('lmn', { delay: 50 });
  await page.locator('#fullscreen-exit').click();
  await expect(focused).toHaveValue('# 2\nChild card\nabclmn');

  // Save and exit edit mode on Ctrl+Enter
  await page.locator('.fullscreen-card-btn').click();
  await focused.pressSequentially(' line', { delay: 50 });
  await page.keyboard.press('Control+Enter');
  await expect(page.locator('#app-fullscreen')).not.toBeVisible();
  await expect(page.locator('#fullscreen-main')).not.toBeVisible();
  // Wait for synced before proceeding
  await expect(page.locator('#save-indicator')).toContainText('Synced');
  await expect(page.locator('textarea')).not.toBeVisible();
  await expect(page.locator(card(2,1,1))).toContainText('cardabclmn line');

  // Save and don't exit edit mode on Ctrl+S
  await page.keyboard.press('ArrowDown');
  await page.keyboard.press('Shift+Enter');
  await expect(page.locator('#fullscreen-main')).toBeVisible();
  await focused.pressSequentially('xyz', { delay: 50 });
  await expect(page.locator('#fullscreen-buttons #save-indicator')).toContainText('Unsaved');
  await page.keyboard.press('Control+s');
  await expect(page.locator('#app-fullscreen')).toBeVisible();
  await expect(page.locator('#fullscreen-main')).toBeVisible();
  await expect(page.locator('#fullscreen-buttons #save-indicator')).toContainText('Synced');

  await page.keyboard.press('Escape');

  // Saved fullscreen changes correctly
  await page.goto(`${config.TEST_SERVER}/${treeIds[1]}`);

  expect(await page.locator(card(1,1,1)).innerHTML()).toContain('<p>Another Test doc</p>');
  expect(await page.locator(card(2,1,1)).innerHTML()).toContain('<p>Child card<br>abclmn line</p>');
  expect(await page.locator(card(2,1,2)).innerHTML()).toContain('<p>Another Child card testxyz</p>');
})
