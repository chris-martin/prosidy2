{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Prosidy
import           Control.Lens
import           System.Environment             ( getArgs )
import qualified Data.Text.IO                  as Text.IO

main :: IO ()
main = do
    args <- getArgs
    let [_, input, output] = filter ((/= "-") . take 1) args
    doc <- readDocument input
    doc
        ^. cosmosOnOf (content . folded) (_BlockTag . content . folded)
        .  _BlockLiteral
        .  filtered (has $ tag . only [keyQ|haskell|])
        .  content
        .  coerced
        .  to (<> "\n")
        &  Text.IO.writeFile output
