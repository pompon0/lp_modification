module Main where

import Test.Tasty (testGroup,defaultMain)
import Test.Tasty.HUnit (Assertion,testCase,(@=?))
import Err

import Control.Lens

main = defaultMain $ testGroup "ErrTest" [
  testCase "for" forTest]

-- for should iterate over lists from head to tail
forTest = do
  want <-r$ [1..5]
  got <- [] & for [1..5] (\i cont [] -> do
    t <- cont []
    r$ i:t)
  want @=? got
