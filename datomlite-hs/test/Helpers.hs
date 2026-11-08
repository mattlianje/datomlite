module Helpers (expectRight, must) where

import Test.Tasty.HUnit (assertFailure)

expectRight :: Show e => Either e a -> IO a
expectRight = either (assertFailure . show) pure

must :: Show e => Either e a -> a
must = either (error . show) id
