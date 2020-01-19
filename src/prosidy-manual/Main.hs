{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
module Main where

import           Development.Shake
import           Development.Shake.FilePath
import           Data.Text                      ( Text )
import           Control.DeepSeq                ( NFData(..) )
import           Data.Binary                    ( Binary(..) )
import           Data.Hashable                  ( Hashable )
import           Data.String                    ( IsString )
import           GHC.Generics                   ( Generic )
import           Control.Exception              ( displayException )

import qualified Data.ByteString.Lazy          as LBS
import qualified Prosidy                       as P
import qualified Prosidy.Manual.TableOfContents
                                               as TOC
import qualified Prosidy.Manual                as Manual

main :: IO ()
main = shakeArgs options $ do
    addOracles

    "all" ~> do
        need ["_out/doc.tar.xz"]

    "manual" ~> do
        pages     <- fmap prosidyToHtml <$> askOracle AllPages
        resources <- fmap ("_out" </>) <$> askOracle AllResources
        let everything = pages ++ resources
        need everything

    "_out/doc.tar.xz" %> \path -> do
        pages     <- fmap prosidyToHtml <$> askOracle AllPages
        resources <- fmap ("_out" </>) <$> askOracle AllResources
        let everything = pages ++ resources
        need everything
        command_ [] "tar" ("cfv" : path : everything)

    "_out/doc/*.html" %> \page -> do
        let src = htmlToProsidy page
        toc <- askOracle TableOfContents
        doc <- askOracle $ DocumentFor src
        putInfo $ unwords ["#", "prosidy:", src, "(for " <> page <> ")"]
        case
                Manual.compile Manual.document
                               (makeRelative "_out/doc" page)
                               toc
                               doc
            of
                Left  err -> fail $ displayException err
                Right ok  -> liftIO $ LBS.writeFile page ok

    "_out/doc/res/*" %> \path -> copyFile' (dropDirectory1 path) path

options :: ShakeOptions
options = shakeOptions

addOracles :: Rules ()
addOracles = do
    addOracle $ \AllResources -> do
        paths <- getDirectoryContents "doc/res"
        pure $ fmap ("doc/res" </>) paths

    addOracle $ \AllPages -> do
        paths <- getDirectoryContents "doc"
        pure . filter ((== ".pro") . takeExtension) . fmap ("doc" </>) $ paths

    addOracleCache $ \(MetadataFor path) -> do
        putInfo $ "Reading metadata at " <> path
        need [path]
        liftIO $ P.readDocumentMetadata path

    addOracleCache $ \(DocumentFor path) -> do
        putInfo $ "Reading document at " <> path
        need [path]
        liftIO $ P.readDocument path

    addOracleCache $ \(EntryFor path) -> do
        putInfo $ "Generating table of contents for " <> path
        document <- askOracle (DocumentFor path)
        pure $ TOC.documentEntry document

    addOracleCache $ \TableOfContents -> do
        putInfo "Generating table of contents"
        pages   <- askOracle AllPages
        entries <- askOracles (fmap EntryFor pages)
        let foldFn page = TOC.insertEntry
                (makeRelative "_out/doc/" $ prosidyToHtml page)
        pure . foldr (uncurry foldFn) mempty $ zip pages entries

    pure ()

prosidyToHtml :: FilePath -> FilePath
prosidyToHtml fp = "_out" </> replaceExtension fp ".html"

htmlToProsidy :: FilePath -> FilePath
htmlToProsidy fp = replaceExtension (dropDirectory1 fp) ".pro"

newtype MetadataFor = MetadataFor FilePath
  deriving newtype (Eq, NFData, Binary, Hashable, IsString)
  deriving stock (Show)

type instance RuleResult MetadataFor = P.Metadata

newtype DocumentFor = DocumentFor FilePath
  deriving newtype (Eq, NFData, Binary, Hashable, IsString)
  deriving stock (Show)

type instance RuleResult DocumentFor = P.Document

newtype EntryFor = EntryFor FilePath
  deriving newtype (Eq, NFData, Binary, Hashable, IsString)
  deriving stock (Show)

type instance RuleResult EntryFor = TOC.Entry

data TableOfContents = TableOfContents
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Binary, Hashable)

type instance RuleResult TableOfContents = TOC.TableOfContents

data AllResources = AllResources
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Binary, Hashable)

type instance RuleResult AllResources = [FilePath]

data AllPages = AllPages
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Binary, Hashable)

type instance RuleResult AllPages = [FilePath]
