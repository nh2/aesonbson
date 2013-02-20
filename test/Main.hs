
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit hiding (Test)

import Data.List


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "Sorting Group 2" [
                testCase "sort7" test_sort7,
                testCase "sort8" test_sort8
            ]
    ]


test_sort7 :: Assertion
test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]

test_sort8 :: Assertion
test_sort8 = error "This test deliberately contains a user error"
