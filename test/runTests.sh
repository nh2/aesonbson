#!/usr/bin/env bash

ghc -package test-framework \
    -package test-framework-hunit -threaded Main.hs -o tests-all
./tests-all --maximum-generated-tests=5000 +RTS -N2
