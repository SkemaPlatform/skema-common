#!/bin/bash

runhaskell Setup configure --user
runhaskell Setup build
runhaskell Setup haddock
runhaskell Setup install
