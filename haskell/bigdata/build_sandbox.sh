#!/bin/bash

set -e
set -x

rm -fr .cabal-sandbox cabal.sandbox.config
cabal sandbox init
cabal install Cabal
cabal install cairo
cabal install --haddock-hyperlink-source --dependencies-only
