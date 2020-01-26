module Prosidy.Test (main) where

import Test.Tasty
import qualified Prosidy.Test.Prosidy

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
    prosidyTests <- Prosidy.Test.Prosidy.tests
    pure $ testGroup "test"
        [ prosidyTests
        ]
