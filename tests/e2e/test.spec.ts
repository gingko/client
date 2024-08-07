import {test, expect} from "@playwright/test";
import { seedWith, setupLifecycleHooks } from "./shared";
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
  // cy.shortcut('{esc}')
  // cy.get('#app-fullscreen').should('not.exist')
  // cy.get('#fullscreen-main').should('not.exist')
  // cy.get('textarea').should('not.exist')
  // cy.getCard(2,1,1).should('contain', 'cardabc')
  // cy.getCard(2,1,2).should('contain', 'card test')
})
