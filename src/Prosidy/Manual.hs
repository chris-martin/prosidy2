{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Manual where

import Control.Applicative (Alternative(..))
import qualified Options.Applicative as O
import Prosidy (readDocument)
import Data.Foldable (foldr', for_)
import Prosidy.Manual.TableOfContents (documentEntry, insertEntry)
import Prosidy.Manual.Compile (compile, document)
import Data.Bifunctor (second)
import Control.Exception (throwIO, displayException, handle, fromException)
import System.Exit (ExitCode, exitFailure)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>), (-<.>))
import qualified System.IO as IO

main :: IO ()
main = handleException $ do
    Opts{inputs, outputDir} <- O.execParser optOpts
    documents <- zip inputs <$> traverse readDocument inputs
    let entries = fmap (second documentEntry) documents
        toc     = foldr' (uncurry insertEntry) mempty entries
    for_ documents $ \(path, doc) ->
        case compile document toc path doc of
            Left e -> throwIO e
            Right ok -> LBS.writeFile (outputDir </> path -<.> "html") ok

handleException :: IO a -> IO a
handleException =
    handle (\e -> case fromException @ExitCode e of
                    Just code -> throwIO code
                    Nothing -> IO.hPutStrLn IO.stderr (displayException e) *> exitFailure)

data Opts = Opts
    { outputDir :: FilePath
    , inputs    :: [FilePath]
    }
  deriving Show

optOpts :: O.ParserInfo Opts
optOpts = O.info (Opts <$> optOutputDir <*> optInputs) $ mconcat
    [
    ]

optInputs :: O.Parser [FilePath]
optInputs = some . O.strArgument $ mconcat
    [ O.help "A document to process."
    , O.metavar "PRO"
    ]

optOutputDir :: O.Parser FilePath
optOutputDir = O.strOption $ mconcat
    [ O.short 'o', O.long "output-root"
    , O.help "The root directory to write resulting HTML in to."
    , O.metavar "HTML"
    ]

-- import           Development.Shake
-- import           Development.Shake.FilePath
-- import           Data.Text                      ( Text )
-- import           Control.DeepSeq                ( NFData(..) )
-- import           Data.Binary                    ( Binary(..) )
-- import           Data.Hashable                  ( Hashable )
-- import           Data.String                    ( IsString )
-- import           GHC.Generics                   ( Generic )
-- import           Control.Exception              ( displayException )

-- import qualified Data.ByteString.Lazy          as LBS
-- import qualified Prosidy                       as P
-- import qualified Prosidy.Manual.TableOfContents
--                                                as TOC
-- import qualified Prosidy.Manual.Compile        as Compile

-- main :: IO ()
-- main = shake options $ do
--     addOracles
--     want ["manual"]

--     "manual" ~> do
--         pages     <- fmap prosidyToHtml <$> askOracle AllPages
--         resources <- fmap ("_out" </>) <$> askOracle AllResources
--         let everything = pages ++ resources
--         need everything

--     "_out/doc.tar.xz" %> \path -> do
--         pages     <- fmap prosidyToHtml <$> askOracle AllPages
--         resources <- fmap ("_out" </>) <$> askOracle AllResources
--         let everything = pages ++ resources
--         need everything
--         command_ [] "tar" ("cfv" : path : everything)

--     "_out/doc/*.html" %> \page -> do
--         let src = htmlToProsidy page
--         toc <- askOracle TableOfContents
--         doc <- askOracle $ DocumentFor src
--         putNormal $ unwords ["#", "prosidy:", src, "(for " <> page <> ")"]
--         case
--                 Compile.compile Compile.document
--                                (makeRelative "_out/doc" page)
--                                toc
--                                doc
--             of
--                 Left  err -> fail $ displayException err
--                 Right ok  -> liftIO $ LBS.writeFile page ok

--     "_out/doc/res/*" %> \path -> copyFile' (dropDirectory1 path) path

-- options :: ShakeOptions
-- options = shakeOptions

-- addOracles :: Rules ()
-- addOracles = do
--     addOracle $ \AllResources -> do
--         paths <- getDirectoryContents "doc/res"
--         pure $ fmap ("doc/res" </>) paths

--     addOracle $ \AllPages -> do
--         paths <- getDirectoryContents "doc"
--         pure . filter ((== ".pro") . takeExtension) . fmap ("doc" </>) $ paths

--     addOracleCache $ \(MetadataFor path) -> do
--         putNormal $ "Reading metadata at " <> path
--         need [path]
--         liftIO $ P.readDocumentMetadata path

--     addOracleCache $ \(DocumentFor path) -> do
--         putNormal $ "Reading document at " <> path
--         need [path]
--         liftIO $ P.readDocument path

--     addOracleCache $ \(EntryFor path) -> do
--         putNormal $ "Generating table of contents for " <> path
--         document <- askOracle (DocumentFor path)
--         pure $ TOC.documentEntry document

--     addOracleCache $ \TableOfContents -> do
--         putNormal "Generating table of contents"
--         pages   <- askOracle AllPages
--         entries <- askOracles (fmap EntryFor pages)
--         let foldFn page = TOC.insertEntry
--                 (makeRelative "_out/doc/" $ prosidyToHtml page)
--         pure . foldr (uncurry foldFn) mempty $ zip pages entries

--     pure ()

-- prosidyToHtml :: FilePath -> FilePath
-- prosidyToHtml fp = "_out" </> replaceExtension fp ".html"

-- htmlToProsidy :: FilePath -> FilePath
-- htmlToProsidy fp = replaceExtension (dropDirectory1 fp) ".pro"

-- newtype MetadataFor = MetadataFor FilePath
--   deriving newtype (Eq, NFData, Binary, Hashable, IsString)
--   deriving stock (Show)

-- type instance RuleResult MetadataFor = P.Metadata

-- newtype DocumentFor = DocumentFor FilePath
--   deriving newtype (Eq, NFData, Binary, Hashable, IsString)
--   deriving stock (Show)

-- type instance RuleResult DocumentFor = P.Document

-- newtype EntryFor = EntryFor FilePath
--   deriving newtype (Eq, NFData, Binary, Hashable, IsString)
--   deriving stock (Show)

-- type instance RuleResult EntryFor = TOC.Entry

-- data TableOfContents = TableOfContents
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (NFData, Binary, Hashable)

-- type instance RuleResult TableOfContents = TOC.TableOfContents

-- data AllResources = AllResources
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (NFData, Binary, Hashable)

-- type instance RuleResult AllResources = [FilePath]

-- data AllPages = AllPages
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (NFData, Binary, Hashable)

-- type instance RuleResult AllPages = [FilePath]
