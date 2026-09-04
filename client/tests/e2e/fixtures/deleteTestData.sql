PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
DELETE FROM tree_snapshots WHERE treeId IN (SELECT trees.id FROM trees WHERE owner='cypress@testing.com');
DELETE FROM cards WHERE treeId IN (SELECT trees.id FROM trees WHERE owner='cypress@testing.com');
DELETE FROM trees WHERE owner='cypress@testing.com';
COMMIT;
