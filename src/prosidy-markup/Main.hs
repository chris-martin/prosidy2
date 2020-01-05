{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main
    ( main
    )
where

import           Prosidy
import           Prosidy.Compile

import           Options.Applicative
import           Data.Text                      ( Text )
import           Control.Exception              ( throwIO
                                                , bracket
                                                )
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Data.Text.Lazy                 ( toStrict )
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Internal           as Blaze

import qualified System.IO                     as IO
import qualified Data.Text.IO                  as Text.IO

import qualified Control.Lens                  as L
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

main :: IO ()
main = do
    opts@Opts {..} <- getOpts
    input <- withFile' IO.stdin inputFile IO.ReadMode Text.IO.hGetContents
    document <- either throwIO pure
        $ parseDocument (maybe "<stdin>" id inputFile) input
    html <- either (fail . show) (pure . toStrict . renderHtml)
        $ compile (compiler standalone breakUsing) document
    withFile' IO.stdout outputFile IO.WriteMode (`Text.IO.hPutStrLn` html)

withFile'
    :: IO.Handle -> Maybe FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile' hdl path mode =
    bracket (maybe (pure hdl) (`IO.openFile` mode) path) IO.hClose

compiler :: Bool -> Text -> Rule Document H.Html
compiler standalone space = mdo
    break <- rule "break" $ pure $ H.text space

    text  <- rule "plain text" $ do
        body <- self
        pure $ H.text body

    literalText <- rule "literal text" $ do
        body <- descend text _Literal
        pure $ H.code body

    block <- choose
        "block context"
        [ _BlockTag @? blockTag
        , _BlockLiteral @? codeBlock
        , _BlockParagraph @? paragraph
        ]

    inline <- choose
        "inline context"
        [_Break @? break, _InlineTag @? inlineTag, _InlineText @? text]

    blockTag <- choose
        "block tag"
        [ _Tagged [keyQ|h|] @? heading
        , _Tagged [keyQ|h+|] @? subheading
        , _Tagged [keyQ|h++|] @? subsubheading
        , _Tagged [keyQ|list|] @? list
        , _Tagged [keyQ|section|] @? section
        , _Tagged [keyQ|quote|] @? quote
        , _Tagged [keyQ|image|] @? blockImage
        ]

    finalBlock <- choose "final block tag" [_BlockParagraph @? paragraph]

    inlineTag  <- choose
        "inline tag"
        [ _Tagged [keyQ|b|] @? bold
        , _Tagged [keyQ|i|] @? italic
        , _Tagged [keyQ|image|] @? image
        , _Tagged [keyQ|link|] @? link
        , _Tagged [keyQ|lit|] @? lit
        ]

    codeBlock <- rule "codeBlock" $ do
        body <- descend literalText content
        pure $ H.pre body

    paragraph <- rule "paragraph" $ do
        body <- descend inline (_Paragraph . L.folded)
        pure $ H.p body

    heading <- rule "heading" $ do
        body <- descend finalBlock (content . L.folded)
        pure $ H.h2 body

    subheading <- rule "subheading" $ do
        body <- descend finalBlock (content . L.folded)
        pure $ H.h3 body

    subsubheading <- rule "subsubheading" $ do
        body <- descend finalBlock (content . L.folded)
        pure $ H.h4 body

    image <- rule "image" $ do
        url   <- req Right [keyQ|url|] "The URL of the image to embed."
        title <- opt Right [keyQ|title|] "The title/alt-text of the image."
        pure $ do
            H.img ! A.src (H.toValue url) ! foldMap (A.title . H.toValue) title

    blockImage <- rule "blockImage" $ do
        url   <- req Right [keyQ|url|] "The URL of the image to embed."
        title <- opt Right [keyQ|title|] "The title/alt-text of the image."
        body  <- descend finalBlock $ content . L.folded
        pure $ do
            H.figure $ do
                H.img
                    ! A.src (H.toValue url)
                    ! foldMap (A.title . H.toValue) title
                H.figcaption body

    bold <- rule "bold" $ do
        body <- descend inline (content . L.folded)
        pure $ H.strong body

    italic <- rule "italic" $ do
        body <- descend inline (content . L.folded)
        pure $ H.em body

    link <- rule "link" $ do
        url  <- req Right [keyQ|url|] "The URL to link to."
        body <- descend inline $ content . L.folded
        pure $ H.a body ! A.href (H.toValue url)

    lit <- rule "literal" $ do
        body <- descend inline $ content . L.folded
        pure $ H.code body

    listItem <- choose "list item"
                       [_BlockTag . _Tagged [keyQ|item|] @? listBody]

    listBody <- rule "list body" $ do
        body <- descend block (content . L.folded)
        pure $ H.li body

    list <- rule "list" $ do
        isOrdered <- prop [keyQ|ord|]
                          "If provided, treat the list as a ordered-list."
        body <- descend listItem (content . L.folded)
        pure $ do
            (if isOrdered then H.ol else H.ul) body

    quote <- rule "quote" $ do
        body <- descend block (content . L.folded)
        pure $ H.blockquote body

    section <- rule "section" $ do
        class_ <- opt Right [keyQ|class|] "Optional classes to be attached."
        body   <- descend block (content . L.folded)
        pure $ H.section body ! foldMap (A.class_ . H.toValue) class_

    rule "document" $ do
        title <- req Right
                     [keyQ|title|]
                     "The document's title, used as the header."
        style <- if standalone
            then opt Right
                     [keyQ|style|]
                     "A stylesheet to attach to the document"
            else pure Nothing
        body <- descend block (content . L.folded)
        pure $ do
            let titleHtml = H.text title
            if standalone
                then H.html $ do
                    H.head $ do
                        H.title titleHtml
                        foldMap
                            (\s ->
                                H.link
                                    ! A.rel "stylesheet"
                                    ! A.type_ "text/css"
                                    ! A.href (H.toValue s)
                            )
                            style
                    H.body $ do
                        H.header $ H.h1 titleHtml
                        H.main body
                else do
                    H.h1 titleHtml
                    body

data Opts = Opts
    { breakUsing :: Text
    , inputFile  :: Maybe FilePath
    , outputFile :: Maybe FilePath
    , standalone :: Bool
    }
  deriving Show

getOpts = execParser $ info optParse optInfo
  where
    optParse = do
        breakUsing <- strOption $ mconcat
            [ help
                "Replace newlines and white-space on either side of tags\
              \ with this sequence."
            , long "break"
            , short 'b'
            , metavar "STR"
            , value " "
            , showDefault
            ]
        inputFile <- optional . strOption $ mconcat
            [ help
                "Read a Prosidy Markup document from this filepath.\
              \ If not provided, standard-input will be used."
            , long "in"
            , short 'i'
            , metavar "FILEPATH"
            ]
        outputFile <- optional . strOption $ mconcat
            [ help
                "Write the output HTML to this filepath.\
              \ If not provided, standard-output will be used."
            , long "out"
            , short 'o'
            , metavar "FILEPATH"
            ]
        standalone <- switch $ mconcat
            [ help "When provided, generate the full HTML document."
            , long "standalone"
            , short 's'
            ]
        pure Opts { .. }

    optInfo = mconcat []
