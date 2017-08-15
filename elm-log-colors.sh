#!/usr/bin/env bash
# Fakes elm-make into printing colors when
# run by elm-webpack-loader
unbuffer node_modules/.bin/elm-make $@
