# e2e database fixtures

`db/*.sqlite` are small, ready-to-go SQLite databases. Each Playwright worker copies
one to `test-results/dbs/db-worker-<N>.sqlite` and starts its own server against that
copy, so tests never touch the dev database in `data/`.

Pick one per spec file:

```ts
test.use({ seed: 'oneTree' });
```

| fixture | contents |
| --- | --- |
| `empty` | the test user, no documents |
| `noUser` | no user and no documents -- for the signup flow, which needs `cypress@testing.com` *not* to exist yet |
| `oneEmptyTree` | one empty document |
| `twoTrees` | two small documents (the default) |
| `fourSmallTrees` | four small documents |
| `oneTree` | one large document (873 cards) |

All of them except `noUser` contain the user `cypress@testing.com` / `testing`, confirmed,
with a trial that expires in 2100 so the fixtures don't go stale.

## Keeping them current

These files are the source of truth — there are no `.sql` seeds to regenerate from.
When the schema changes, apply the same migration you'd run on production to each
fixture:

```sh
for f in client/tests/e2e/fixtures/db/*.sqlite; do
  sqlite3 "$f" < server/scripts/db/0002-whatever.sql
done
```

Then commit the updated files. Note that new tables added via `CREATE TABLE IF NOT
EXISTS` at server boot need no migration — the server creates them in the worker's copy
on startup.
