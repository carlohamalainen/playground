#!/bin/bash

.cabal-sandbox/bin/pandoc -t revealjs --variable transition="none" --include-in-header=leftalign.css -s talk.md -o talk.html
