import config from '../../config.js';
import {execSync} from "child_process";

const cwd = process.cwd();
const dbPath = `${cwd}/../data/data.sqlite`;
const oldFixtures = `${cwd}/cypress/fixtures`;
const fixtures = `${cwd}/tests/e2e/fixtures`;

export function setupLifecycleHooks(test) {
  test.afterEach(({}, testInfo) => {
    if (testInfo.status === "failed" && process.env.CI !== "true") {
      execSync(`notify-send -u critical "Test Failed" "${testInfo.title}"`);
    }
  });

  test.afterAll(({}, testInfo) => {
    if (testInfo.status === "failed" && process.env.CI !== "true") {
      execSync(`notify-send -u critical "Test Failed" "${testInfo.title}"`);
    }
  });
}

export async function signupWith(userEmail: string, seedName: string) {
  const coucdbDelete = await fetch(`${config.TEST_SERVER}/test/user`, { method: 'DELETE' });
  console.log(await coucdbDelete.text());
  deleteTestData();

  const response = await fetch(`${config.TEST_SERVER}/signup`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({ email: userEmail, password: 'testing'})
  })

  if (response.status !== 200) {
    console.log(await response.text());
    throw new Error('Failed to signup');
  }

  await seedSqliteDb(seedName);
}

export function seedWith(seedName: string) {
  console.log(`Seeding with ${seedName}`, dbPath, oldFixtures);
  const seedRes = execSync(`sqlite3 ${dbPath} < ${oldFixtures}/${seedName}.sql`);
  console.log(seedRes.toString());
}

async function seedSqliteDb(seedName: string) {
  const seedFilePath = `${oldFixtures}/${seedName}.sql`;
  console.log(`dbPath: ${dbPath}\nseedFilePath: ${seedFilePath}`);
  execSync(`sqlite3 ${dbPath} < ${seedFilePath}`);
}

function deleteTestData() {
  const cmd = execSync(`sqlite3 ${dbPath} < ${oldFixtures}/deleteTestData.sql`);
  console.log(cmd.toString());
}