#!/bin/bash
set -ev # exit with nonzero exit code if anything fails

if [[ "$TRAVIS_BRANCH" != 'master' ]]; then
  exit 0;
fi

# clear the documentation directory
rm -rf docs || exit 0;

# build the documentation files
scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --html --dest ./docs ./scribblings/threading.scrbl

# go to the documentation directory and create a *new* Git repo
cd docs
git init

# inside this git repo we'll pretend to be a new user
git config user.name 'Travis CI'
git config user.email 'lexi.lambda@gmail.com'

# The first and only commit to this new Git repo contains all the
# files present with the commit message "Deploy to GitHub Pages".
git add .
git commit -m 'Deploy to GitHub Pages'

# Force push from the current repo's master branch to the remote
# repo. (All previous history on the branch will be lost, since we are
# overwriting it.) We redirect any output to /dev/null to hide any sensitive
# credential data that might otherwise be exposed.
git push --force --quiet "https://${GH_TOKEN}@${GH_REF}" master:gh-pages > /dev/null 2>&1
