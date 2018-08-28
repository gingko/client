# Gingko 2 [![Build Status](https://travis-ci.org/gingko/client.svg?branch=master)](https://travis-ci.org/gingko/client)
This is the desktop version, ground-up rewrite of [GingkoApp.com](https://gingkoapp.com). The latest version of this is available for download at [gingko.io](https://gingko.io). (Linux, Windows, and Mac).

# Installation & Dev Environment

Prerequisites:

* Git : https://git-scm.com/downloads
* Node : https://nodejs.org/en/
* Elm-Platform : https://guide.elm-lang.org/install.html

```bash
# 1. Build code:
npm install
npm start

# 2. In separate terminal:
npm run electron
```

# Contributions Welcome

See [CONTRIBUTING.md](./CONTRIBUTING.md) for a guide to get started.


# Release process

(for version 2.x.y)

1. In Github, draft a new release v2.x.y
  - Title: 2.x.y
  - Description: (paste from changelog)
2. Update version number in:
  - package.json
  - app/package.json
  - CHANGELOG.md
3. Test build artifacts by using [trytravis](https://github.com/SethMichaelLarson/trytravis)
4. git commit -m"Version 2.x.y release"
5. git push
6. Wait for builds on [Travis CI](https://travis-ci.org/gingko/client/builds) (approx 8 min)
7. Update links in site/index.html
8. Check release files on https://github.com/gingko/client/releases
9. When done, publish the release


### To Improve

* Use Git-LFS, because I seem to hit rate limits when downloading large files.
* Is `osx_image: xcode9.2` really necessary? Seems to slow down builds, and wasn't needed before
* Verify that Travis is using "cache" (download at 20MB/s +)
