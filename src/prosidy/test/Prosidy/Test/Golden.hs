{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Test.Golden
    ( test
    )
where

import qualified Prosidy                       as P
import qualified Paths_prosidy                 as Paths
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LBS

import           Test.Tasty
import           Test.Tasty.Golden

import           Control.Exception

testNames :: [String]
testNames = ["gamut", "literal"]

test :: IO TestTree
test = do
    dataDir <- Paths.getDataDir
    let inputDir  = dataDir <> "/test-data/golden/input/"
        outputDir = dataDir <> "/test-data/golden/output/"
    pure . testGroup "Golden" $ fmap (makeTest inputDir outputDir) testNames


makeTest :: FilePath -> FilePath -> String -> TestTree
makeTest inDir outDir name = goldenVsString name out (readToJson inp)
  where
    inp = inDir <> name <> ".pro"
    out = outDir <> name <> ".golden"

readToJson :: FilePath -> IO LBS.ByteString
readToJson inputPath = do
    document <- P.readDocument inputPath `catch` \e -> do
        putStrLn $ displayException (e :: SomeException)
        throwIO e
    let json = Aeson.encode document
    pure $ json <> "\n"
