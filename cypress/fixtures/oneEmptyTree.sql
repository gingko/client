PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
INSERT OR REPLACE INTO trees VALUES('9y9ve',NULL,'couchdb','cypress@testing.com','[]',NULL,1608237474302,1680531192924,NULL);
INSERT OR REPLACE INTO users VALUES('cypress@testing.com','d7cc8db0933d61b20591d84282fb4b2a','012e1e75f464154411db4f1a3e6fded149e0c30e',unixepoch()*1000,unixepoch()*1000,'trial:' || CAST(1000*(unixepoch() + 14*24*60*60) AS TEXT),'en');
COMMIT;
