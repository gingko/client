![](./docs/images/screenshot-alien-screenplay.png)

# Gingko Writer [![Web Deploy](https://github.com/gingko/client/actions/workflows/web-deploy.yml/badge.svg)](https://github.com/gingko/client/actions/workflows/web-deploy.yml)

Writing software to help organize and draft complex documents. Anything from novels and screenplays to legal briefs and graduate theses.

This is a ground-up rewrite of [GingkoApp.com](https://gingkoapp.com). The latest version is available online at [gingkowriter.com](https://gingkowriter.com).

The desktop version (on branch [desktop](https://github.com/gingko/client/tree/desktop)), is currently **well behind** the web app version. It will eventually be brought up to par, but if you need it now, it's available to download on [the releases page](https://github.com/gingko/client/releases) (for Linux, Windows, and Mac).

## Contributions Welcome!

If you want to help **translate Gingko Writer**, you can join [the translation project](https://poeditor.com/join/project/k8Br3k0JVz).

For code contributions, see [CONTRIBUTING.md](./CONTRIBUTING.md) for a guide to getting started.

---

### Installation & Dev Environment

#### 1. Install Prerequisites
- [Node.js](https://nodejs.org)
- [Bun.sh](https://bun.sh)
- [SQlite](https://sqlite.org)
- [CouchDB](https://couchdb.apache.org)*
  - Note down your admin username and password for the next step.

I can't provide detailed instructions for installing these, because each system is different.

\* _this dependency will be removed once all user documents are migrated to SQLite DB._

#### 2. Project Directories

```
git clone git@github.com:gingko/client.git
git clone git@github.com:gingko/server.git
mkdir data
```
After this you should have all three directories side-by-side: client, server, and data.

#### 3. Client setup

```
cd client
bun i
cp config-example.js config.js
bun run newwatch
```

#### 4. Server setup

In a new terminal, navigate to the `/server` directory, and do the following:
```
npm i
cp config-example.js config.js
sed -i 's/couchusername/your_couchdb_admin_username/' config.js
sed -i 's/couchpassword/your_couchdb_admin_password/' config.js
npm run build
npm start
```

Now you should be able to visit http://localhost:3000 and use your local Gingko Writer install.
