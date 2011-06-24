#!/bin/bash

runhaskell Setup configure --user
if [ "$?" = "1" ]
then
    exit 1
fi
runhaskell Setup build
if [ "$?" = "1" ]
then
    exit 1
fi
runhaskell Setup haddock
if [ "$?" = "1" ]
then
    exit 1
fi
runhaskell Setup install
if [ "$?" = "1" ]
then
    exit 1
fi
