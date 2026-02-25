#!/bin/sh
#
# gitmezip
# Gets the head of the current directory and creates a zip file of it's HEAD
# Run from the repo's directory
# 
GIT_HASH=$(git rev-parse HEAD)
REPO_DIR_NAME=${PWD##*/}

git archive --format=zip --output ../${REPO_DIR_NAME}-${GIT_HASH}.zip HEAD
