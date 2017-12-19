#!/usr/bin/env bash
# Fakes elm-make into printing colors when
# run by elm-webpack-loader.

# Requires unbuffer to be installed:

# On linux/ubuntu:
# $ sudo apt-get install expect-dev

# On macOS:
# $ brew install expect
unbuffer node_modules/.bin/elm-make $@
