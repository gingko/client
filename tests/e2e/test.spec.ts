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
  await page.goto(`${config.TEST_SERVER}/${treeIds[1]}`);

  expect(page).toHaveURL(`${config.TEST_SERVER}/${treeIds[1]}`);

  await expect(page.locator('h1')).toHaveText('New Tree');
})
