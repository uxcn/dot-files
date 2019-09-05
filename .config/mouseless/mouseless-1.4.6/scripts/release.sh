#!/bin/bash

git checkout master
git merge dev
git tag -a $1 -m $1
git tag push origin $1

