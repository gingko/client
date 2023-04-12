PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
DELETE FROM cards WHERE treeId IN (SELECT trees.id FROM trees WHERE owner='cypress@testing.com');
DELETE FROM trees WHERE owner='cypress@testing.com';
DELETE FROM users WHERE id='cypress@testing.com';
COMMIT;
