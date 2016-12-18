#!/bin/bash
set -e
lein clean
lein cljsbuild once min
cd resources/public
git init
git add .
git commit -m "Deploy to GH Pages"
git push --force --quiet "git@github.com:ccann/chequers.git" master:gh-pages
rm -rf resources/public/.git
