{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NamedFieldPuns #-}
module Prosidy.Manual.Opts (Opts(..), getOpts) where

import qualified Options.Applicative           as Opt

data Opts = Opts
    { dbDir  :: FilePath
    , outDir :: FilePath
    , srcDir :: FilePath
    }
  deriving Show

getOpts :: IO Opts
getOpts = Opt.execParser parseOpts

parseOpts :: Opt.ParserInfo Opts
parseOpts = Opt.info (Opt.helper <*> parse) $ mconcat []
  where
    parse = do
        dbDir <- Opt.strOption $ mconcat
            [ Opt.short 'd'
            , Opt.long "database"
            , Opt.help "The directory which will hold the build database."
            , Opt.value ".prosidy-manual"
            , Opt.showDefault
            , Opt.metavar "database-directory"
            ]
        outDir <- Opt.strOption $ mconcat
            [ Opt.short 'o'
            , Opt.long "out"
            , Opt.help "The root directory to write files into."
            , Opt.value ".out/doc"
            , Opt.showDefault
            , Opt.metavar "output-directory"
            ]
        srcDir <- Opt.strArgument $ mconcat
            [ Opt.help "The root input directory."
            , Opt.value "doc"
            , Opt.showDefault
            , Opt.metavar "input-directory"
            ]
        pure Opts { dbDir, outDir, srcDir }
