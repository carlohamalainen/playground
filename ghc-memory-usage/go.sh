#!/bin/bash

set -u
set -e

./compile.sh

rm -f runs.csv runs.txt prof_*

echo "bytes,string,bytestring,bytestringchar8,lazybytestring,lazybytestringchar8" > runs.csv

./runreadfile.sh  1000
./runreadfile.sh  2000
./runreadfile.sh  3000
./runreadfile.sh  4000
./runreadfile.sh  5000
./runreadfile.sh  6000
./runreadfile.sh  7000
./runreadfile.sh  8000
./runreadfile.sh  9000
./runreadfile.sh 10000

cat runs.txt | sed 's/ /,/g' >> runs.csv
