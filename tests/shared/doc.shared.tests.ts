import { test, expect } from '@playwright/test'

export default function (page) {
  test.skip('exported test', async () => {
    expect(await page.locator('#document').count()).toEqual(1)
  })
}
