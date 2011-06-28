#!/bin/bash

runhaskell Setup configure --user --flags=testing
if [ "$?" = "1" ]
then
    exit 1
fi

runhaskell Setup build
if [ "$?" = "1" ]
then
    exit 1
fi

rm -f *.tix
dist/build/skema-common-test/skema-common-test
if [ "$?" = "1" ]
then
    exit 1
fi

HPCDIR="dist/hpc-html"
if [ ! -d $HPCDIR ]
then
    mkdir -p $HPCDIR
fi
hpc report *.tix
hpc markup --destdir=$HPCDIR *.tix
set -e
