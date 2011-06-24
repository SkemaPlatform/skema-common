#!/bin/bash

runhaskell Setup configure --user --flags=testing
runhaskell Setup build
rm -f *.tix
dist/build/skema-common-test/skema-common-test
hpc report *.tix
hpc markup *.tix
