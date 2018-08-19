#!/bin/bash

set -u
set -e

ghc readfile.hs -Wall -O2 -rtsopts
