#!/bin/bash
cd production/client
git pull https://git@github.com/gingko/client
npm run build
cd ../server
pm2 restart index.js
