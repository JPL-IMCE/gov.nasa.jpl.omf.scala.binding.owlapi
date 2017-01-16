#!/bin/bash

set -ev

. $(dirname $0)/travis-decode.sh

# Deploy if TRAVIS_TAG is set.
# Error if TRAVIS_SECURE_ENV_VARS is false

[ -z "${TRAVIS_TAG}" ] && exit 0;

chmod 600 local.*
eval `ssh-agent -s`
ssh-add local.deploy_key
git config --global push.default simple
git config --global user.email "nicolas.f.rouquette@jpl.nasa.gov"
git config --global user.name "Travis CI (on behalf of Nicolas F. Rouquette)"

sbt -jvm-opts travis/jvmopts.compile -Dproject.version=$TRAVIS_TAG compile test publishSigned ghpagesPushSite

