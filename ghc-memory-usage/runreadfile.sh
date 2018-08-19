#!/bin/bash

set -u
set -e

size=$1

dd if=/dev/zero of=input bs=1M count=${size} 2>/dev/null > /dev/null

./readfile +RTS -tprof_string_${size}                   -- string                   input
./readfile +RTS -tprof_bytestring_${size}               -- bytestring               input
./readfile +RTS -tprof_bytestring-char8_${size}         -- bytestring-char8         input
./readfile +RTS -tprof_lazy_bytestring_${size}          -- lazy-bytestring          input
./readfile +RTS -tprof_lazy_bytestring-char8_${size}    -- lazy-bytestring-char8    input

rm -f input

string=`cat prof_string_${size}                             | cut -f 2 -d ' ' | tail -1`
bytestring=`cat prof_bytestring_${size}                     | cut -f 2 -d ' ' | tail -1`
bytestringchar8=`cat prof_bytestring-char8_${size}          | cut -f 2 -d ' ' | tail -1`
lazybytestring=`cat prof_lazy_bytestring_${size}            | cut -f 2 -d ' ' | tail -1`
lazybytestringchar8=`cat prof_lazy_bytestring-char8_${size} | cut -f 2 -d ' ' | tail -1`

echo $(( ${size} * 1024*1024 )) ${string} ${bytestring} ${bytestringchar8} ${lazybytestring} ${lazybytestringchar8} >> runs.txt
