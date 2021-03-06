#!/bin/bash

PS4='($LINENO)+ '
set -x
set -e

export sandbox=/scratch/sandboxes/prob-monad

rm -fr $sandbox cabal.sandbox.config dist

cabal sandbox init --sandbox=${sandbox}

# cabal sandbox add-source /home/carlo/Desktop/ghc-mod

cabal install --enable-documentation --haddock-hyperlink-source --dependencies-only # Is this necessary? Why not just cabal install?
cabal install
