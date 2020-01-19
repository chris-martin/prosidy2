{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Prosidy
import           Data.List.NonEmpty             ( NonEmpty
                                                , nonEmpty
                                                )
import           Control.Lens
import qualified Data.Text.IO                  as Text.IO
import qualified Options.Applicative           as A
import qualified Options.Applicative.Builder   as AB
import qualified System.IO                     as IO
import           Data.Foldable                  ( for_ )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Control.Applicative            ( Alternative(..)
                                                , optional
                                                )
import qualified Data.HashSet                  as HashSet

main :: IO ()
main = do
    options  <- getOptions
    document <- readDocument $ inputPath options
    let lits =
            document
                ^.. allBlocks
                .   _BlockLiteral
                .   filtered (optionsFilter options)
                .   content
    IO.withFile (outputPath options) IO.WriteMode $ \hdl -> for_ lits $ \lit ->
        do
            for_ (sourceLabelFor options lit)
                $ \label -> Text.IO.hPutStrLn hdl label
            Text.IO.hPutStrLn hdl $ lit ^. content
  where
    allBlocks = cosmosOnOf (content . folded) (_BlockTag . content . folded)

data Options = Options
   { sourceLabel :: Maybe Text
   , tagFilter   :: Maybe (NonEmpty Key)
   , inputPath   :: FilePath
   , outputPath  :: FilePath
   } deriving (Show)

getOptions :: IO Options
getOptions = A.execParser $ A.info parse info
  where
    parse :: A.Parser Options
    parse = do
        AB.abortOption A.ShowHelpText $ mconcat
            [A.long "help", A.help "Show this help text", A.hidden]
        tagFilter <- fmap nonEmpty . many . A.option readKey $ mconcat
            [ A.short 't'
            , A.long "tag"
            , A.help "Extract only literals matching this tag."
            , A.metavar "TAG"
            ]
        sourceLabel <- optional . A.strOption $ mconcat
            [ A.short 'h'
            , A.long "label"
            , A.help "The name of the file that should be displayed to the user"
            , A.metavar "LABEL"
            ]
        inputPath <- A.strArgument $ mconcat
            [ A.help "The actual location of the literate file to process"
            , A.metavar "INPUT"
            ]
        outputPath <- A.strArgument $ mconcat
            [ A.help "The location where the extract source will be written"
            , A.metavar "OUTPUT"
            ]
        pure Options { .. }

    info :: A.InfoMod Options
    info = mconcat
        [ A.progDesc
              "A program to extract source code from literate\
                     \ programming files written in Prosidy."
        ]

    readKey :: A.ReadM Key
    readKey = A.maybeReader (toKey . Text.pack)

sourceLabelFor :: Options -> Literal -> Maybe Text
sourceLabelFor Options { sourceLabel } lit = do
    label      <- sourceLabel
    lineNumber <- lit ^? location . sourceLocationLine
    pure $ mconcat
        ["{-# LINE ", Text.pack $ show lineNumber, " \"", label, "\" #-}"]

optionsFilter :: Options -> LiteralTag -> Bool
optionsFilter Options { tagFilter } = case tagFilter of
    Nothing -> const True
    Just tags ->
        let set = foldMap HashSet.singleton tags
            check x = HashSet.member (x ^. tag) set
        in  check
