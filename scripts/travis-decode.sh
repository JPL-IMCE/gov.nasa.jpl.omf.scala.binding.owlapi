#!/bin/bash

set -ev

[ ! ${TRAVIS_SECURE_ENV_VARS} ] && exit 255;

openssl aes-256-cbc -md md5 -pass pass:$ENCRYPTION_PASSWORD -in secring.gpg.enc -out local.secring.gpg -d
openssl aes-256-cbc -md md5 -pass pass:$ENCRYPTION_PASSWORD -in pubring.gpg.enc -out local.pubring.gpg -d
openssl aes-256-cbc -md md5 -pass pass:$ENCRYPTION_PASSWORD -in credentials.sbt.enc -out local.credentials.sbt -d
openssl aes-256-cbc -md md5 -pass pass:$ENCRYPTION_PASSWORD -in deploy_key.enc -out local.deploy_key -d
