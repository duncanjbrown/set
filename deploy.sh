#!/usr/bin/env bash
set -e

cd public
git init
printf "js/cljs-runtime\njs/manifest.edn" > .gitignore
git add .
git commit -m "Deploy to GitHub Pages"
git push --force --quiet "git@github.com:duncanjbrown/set.git" main:gh-pages
rm -rf .git
rm .gitignore
