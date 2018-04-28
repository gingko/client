# Gingko 2 [![Build Status](https://travis-ci.org/gingko/client.svg?branch=master)](https://travis-ci.org/gingko/client) [![Issue Stats](http://issuestats.com/github/Gingko/client/badge/pr?style=flat)](http://issuestats.com/github/Gingko/client) [![Issue Stats](http://issuestats.com/github/Gingko/client/badge/issue?style=flat)](http://issuestats.com/github/Gingko/client)
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

(for version 2.x.y)

1. In Github, draft a new release v2.x.y
  - Title: 2.x.y
  - Description: (paste from changelog)
2. Test build artifacts by using [trytravis](https://github.com/SethMichaelLarson/trytravis)
3. Update version number in:
  - package.json
  - app/package.json
  - CHANGELOG.md
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
* Improve DMG installer image/background, etc
