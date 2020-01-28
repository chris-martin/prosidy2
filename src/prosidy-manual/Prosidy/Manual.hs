{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
module Prosidy.Manual where

import           Development.Shake.Classes
import           Development.Shake              ( (%>)
                                                , (<//>)
                                                , (~>)
                                                , (|%>)
                                                )
import qualified Development.Shake             as Shake
import           Development.Shake.FilePath     ( (</>)
                                                , (-<.>)
                                                , makeRelative
                                                )
import           GHC.Generics                   ( Generic )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Control.Exception              ( throwIO )

import           Control.Lens.Operators
import           Data.Text.Lens                 ( packed )
import qualified Control.Lens                  as L
import qualified Data.ByteString.Lazy          as LBS

import           Prosidy
import           Prosidy.Manual.Compile         ( compile
                                                , document
                                                )
import           Prosidy.Manual.Opts
import           Prosidy.Manual.Slug
import           Prosidy.Manual.TableOfContents

main :: IO ()
main = do
    opts <- getOpts
    Shake.shake (mainOpts opts) $ do
        installOracles opts
        mainBuild opts

mainOpts :: Opts -> Shake.ShakeOptions
mainOpts Opts { dbDir, outDir } = Shake.shakeOptions
    { Shake.shakeFiles         = dbDir
    , Shake.shakeThreads       = 0
    , Shake.shakeAbbreviations = [(outDir, "$OUTPUT")]
    , Shake.shakeProgress      = Shake.progressSimple
    }

mainBuild :: Opts -> Shake.Rules ()
mainBuild Opts { outDir, srcDir } = do
    Shake.want ["all"]

    "all" ~> do
        Shake.need ["manual", "resources"]

    "manual" ~> do
        prosidySources <- Shake.getDirectoryFiles srcDir ["//*.pro"]
        Shake.need $ fmap (\file -> outDir </> file -<.> "html") prosidySources

    "resources" ~> do
        manualResources <- Shake.getDirectoryFiles
            srcDir
            ["//*.js", "//*.css", "//*.svg"]
        Shake.need $ fmap (\file -> outDir </> file) manualResources

    outDir <//> "*.html" %> \path -> do
        Shake.putNormal $ "# Building " <> path
        let base = makeRelative outDir path
            src  = srcDir </> base -<.> "pro"
        tocFiles <- Shake.askOracle Contents
        doc      <- Shake.askOracle $ DocumentFor src
        Shake.liftIO $ do
            toc   <- either throwIO pure $ tableOfContents tocFiles base doc
            bytes <- either throwIO pure $ compile document toc doc
            LBS.writeFile path bytes

    [outDir <//> "*.css", outDir <//> "*.js", outDir <//> "*.svg"] |%> \path ->
        do
            Shake.putNormal $ "# Building " <> path
            let src = srcDir </> makeRelative outDir path
            Shake.copyFile' src path

installOracles :: Opts -> Shake.Rules ()
installOracles opts@Opts { srcDir } = do
    Shake.addOracleCache $ \(MetadataFor path) -> do
        Shake.need [path]
        Shake.putNormal $ "# Reading metadata for " <> show path
        Shake.liftIO $ readDocumentMetadata path

    Shake.addOracleCache $ \(DocumentFor path) -> do
        Shake.need [path]
        Shake.putNormal $ "# Reading document at " <> show path
        Shake.liftIO $ readDocument path

    Shake.addOracleCache $ \Contents -> do
        Shake.putNormal "# Building table of contents"
        files <- Shake.getDirectoryFiles srcDir ["//*.pro"]
        Shake.need $ fmap (srcDir <//>) files
        foldMap (tocForFile opts) files

    pure ()

tocForFile :: Opts -> FilePath -> Shake.Action TOCFiles
tocForFile Opts { srcDir } path = do
    meta <- Shake.askOracle . MetadataFor $ srcDir </> path
    let thisPrio =
            fromMaybe 0
                $  meta
                ^? setting "priority"
                .  traverse
                .  L.re packed
                .  L._Show
        thisSlug = FileSlug thisPrio (path -<.> "html")
    pure . TOCFiles $ Map.singleton thisSlug meta

data Contents = Contents
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData, Binary)

type instance Shake.RuleResult Contents = TOCFiles

newtype MetadataFor = MetadataFor FilePath
  deriving newtype (Show, Eq, Hashable, NFData, Binary)

type instance Shake.RuleResult MetadataFor = Metadata

newtype DocumentFor = DocumentFor FilePath
  deriving newtype (Show, Eq, Hashable, NFData, Binary)

type instance Shake.RuleResult DocumentFor = Document
