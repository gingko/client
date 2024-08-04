import config from "../../config.js";
import { test as setup, expect } from '@playwright/test';
import { execSync } from "child_process";

const cwd = process.cwd();
const dbPath = `${cwd}/../data/data.sqlite`;
const oldFixtures = `${cwd}/cypress/fixtures`;
const userFile = `${cwd}/tests/e2e/.auth/user.json`;

setup('signup test user', async ({ page }) => {
  // Delete test user data from  SQLite
  execSync(`sqlite3 ${dbPath} < ${oldFixtures}/deleteTestData.sql`);
  // Delete test user's CouchDB
  await page.request.delete(`${config.TEST_SERVER}/test/user`);

  await page.goto(config.TEST_SERVER);
  await page.waitForLoadState('domcontentloaded');

  await expect(page.locator('text=Signup')).toBeVisible();

  const signupRes = await page.request.post(`${config.TEST_SERVER}/signup`, {data: { email: 'cypress@testing.com', password: 'testing' }});

  await page.evaluate(() => {
    console.log('setting localStorage');
    localStorage.setItem('gingko-session-storage', JSON.stringify({ "email": "cypress@testing.com", "language": "en" }));
    console.log('localStorage set', localStorage.getItem('gingko-session-storage'));
    return true;
  });

  // Check if cookies are set in the browser context
  const cookies = await page.context().cookies();
  expect(cookies).not.toEqual([]);

  await page.context().storageState({ path: userFile });
});