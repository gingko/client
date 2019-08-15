#!/usr/bin/env bash
# Fakes elm-make into printing colors when
# run by elm-webpack-loader.

# For elm compiler colors, requires unbuffer to be installed:

# On linux/ubuntu:
# $ sudo apt-get install expect-dev

# On macOS:
# $ brew install expect
#
#
if command -v unbuffer; then
  tput reset; unbuffer node_modules/.bin/elm $@
else
  tput reset; node_modules/.bin/elm $@
fi
