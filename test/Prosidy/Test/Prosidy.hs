{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prosidy.Test.Prosidy (tests) where

import qualified Prosidy

import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Control.Exception (handle, displayException)

import qualified Paths_prosidy as Paths
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as Aeson.Diff
import qualified Data.Aeson.Encode.Pretty as Aeson.Pretty

import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8With)

tests :: IO TestTree
tests = do
    goldenDir   <- (</> "golden") <$> Paths.getDataDir
    goldenTests <- Dir.listDirectory goldenDir
    pure . testGroup "prosidy" $ makeTest goldenDir <$> goldenTests

makeTest :: FilePath -> String -> TestTree
makeTest goldenDir name =
    goldenTest name 
        (catchErrors getJSON) 
        (catchErrors parsePro)
        compareDocs
        writeGolden
  where
    getJSON :: IO Aeson.Value
    getJSON = do
        bytes <- BS.readFile $ goldenDir </> name </> "output.json"
        case Aeson.eitherDecode' . BS.Lazy.fromStrict $ bytes of
            Left e   -> fail $ "Failed to parse JSON: " <> e
            Right ok -> pure ok

    parsePro :: IO Aeson.Value
    parsePro = Aeson.toJSON <$>
        Prosidy.readDocument (goldenDir </> name </> "input.pro") 

    compareDocs :: Aeson.Value -> Aeson.Value -> IO (Maybe String)
    compareDocs gold test | gold == test = pure Nothing
    compareDocs gold test = 
      let
        diff   = Aeson.Diff.diff (Aeson.toJSON gold) (Aeson.toJSON test)
        pretty = Aeson.Pretty.encodePretty diff
      in
        pure . Just . Text.Lazy.unpack $ 
            "Golden test failed. The diff is included below:\n" <>
            decodeUtf8With (\_ _ -> Just '\65533') pretty

    writeGolden :: Aeson.Value -> IO ()
    writeGolden doc = BS.Lazy.writeFile (goldenDir </> name </> "output.json") $
        Aeson.Pretty.encodePretty doc

catchErrors :: IO a -> IO a
catchErrors = handle $ \(e :: Prosidy.Failure) -> fail $ displayException e