import { test, expect } from '@playwright/test'

export default function (page) {
  test('exported test', async () => {
    expect(await page.locator('#document').count()).toEqual(1)
  })
}
