#!/bin/bash

cd `git rev-parse --show-toplevel`
cd docs
asciidoctor -v  -B docs/ *.adoc  
