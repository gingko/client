# Gingko 2 [![Build Status](https://travis-ci.org/gingko/client.svg?branch=master)](https://travis-ci.org/gingko/client)
This is the desktop version, ground-up rewrite of [GingkoApp.com](https://gingkoapp.com). The latest version of this is available for download at [gingko.io](https://gingko.io). (Linux, Windows, and Mac).

This code has only recently been open-sourced, so documentation is sparse.
Please bear with us, while we improve it.

# Installation & Dev Environment

Prerequisites:

* Git : https://git-scm.com/downloads
* Node : https://nodejs.org/en/
* Elm-Platform : https://guide.elm-lang.org/install.html

```bash
# 1. Build code:
yarn install
yarn start

# 2. In separate terminal:
yarn run electron
```


# Release process

(for example 0.8.6)

1. Update version in:
  - package.json
  - app/package.json
  - CHANGELOG.md
2. In Github, draft a new release v0.8.6
  - Title: 0.8.6
  - Description: (paste from changelog)
3. git commit
4. git push
5. Wait for builds on Travis CI (approx 7 min)
6. Check release files on https://github.com/gingko/client/releases
7. When done, publish the release
8. Update links in site/index.html


### To Improve

* Use Git-LFS, because I seem to hit rate limits when downloading large files.
* Is `osx_image: xcode9.2` really necessary? Seems to slow down builds, and wasn't needed before
* Verify that Travis is using "cache" (download at 20MB/s +)
* Improve DMG installer image/background, etc
