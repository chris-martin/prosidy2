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
