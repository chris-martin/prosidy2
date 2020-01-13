{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Main
    ( main
    )
where

import           Prosidy
import           Prosidy.Compile

import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity)
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
import Data.Foldable (for_)

import qualified Control.Lens                  as L
import Control.Lens.Operators
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

main :: IO ()
main = do
    opts@Opts {..} <- getOpts
    input <- withFile' IO.stdin inputFile IO.ReadMode Text.IO.hGetContents
    document <- either throwIO pure
        $ parseDocument (fromMaybe "<stdin>" inputFile) input
    case compile (compiler standalone breakUsing) document of
        Left  err  -> fail $ show err
        Right html -> do
            withFile' IO.stdout outputFile IO.WriteMode $ \handle ->
                Text.IO.hPutStrLn handle . toStrict . renderHtml $ html

withFile'
    :: IO.Handle -> Maybe FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile' hdl path mode =
    bracket (maybe (pure hdl) (`IO.openFile` mode) path) IO.hClose

compileBlockTag :: Item (Spanned Paragraph) Identity H.Html -> Item Block Identity H.Html -> ProductT (Spanned BlockTag) Identity H.Html
compileBlockTag paragraph block = do
    textOnly <- blockRule "text only block"
        paragraph
        (disallow "literal tags not allowed in this context")
        (disallow "nested block tags are not allowed in this context")

    heading <- tagRule "h" "first-level heading" $
        H.h2 <$> children textOnly

    subheading <- tagRule "h+" "second-level heading" $ do
        H.h3 <$> children textOnly

    subsubheading <- tagRule "h++" "third-level heading" $ do
        H.h4 <$> children textOnly

    blockImage <- tagRule "image" "block image with optional caption" $ do
        url   <- req Right "url" "The URL of the image to embed."
        title <- opt Right "title" "The title/alt-text of the image."
        body  <- children textOnly
        pure $ do
            H.figure $ do
                H.img
                    ! A.src (H.toValue url)
                    ! foldMap (A.title . H.toValue) title
                H.figcaption body

    blockQuote <- tagRule "quote" "a blockquote" $ do
        body <- children block
        pure $ H.blockquote body

    list <- compileList block

    tags "block tags" 
        [ heading
        , subheading
        , subsubheading
        , list
        , blockImage
        , blockQuote
        ]

compileList :: Item Block Identity H.Html -> SumT (Spanned BlockTag) Identity H.Html
compileList block = do
    item <- tagRule "item" "A single item within a list." $ do
        content <- children block
        pure $ H.li content

    onlyItems <- tags "inside of a list" [item]

    insideList <- blockRule "inside of a list"
        (disallow "paragraphs are not permitted in this context")
        (disallow "literal blocks are not permitted in this context")
        onlyItems

    tagRule "list" "A listing. All children must be 'item' block tags." $ do
        isOrdered <- prop "ord" "If provided, the list is numerically ordered."
        content   <- children insideList
        pure $ (if isOrdered then H.ol else H.ul) content

compileInlineTag :: Item Inline Identity H.Html -> ProductT (Spanned InlineTag) Identity H.Html
compileInlineTag inline = do
    image <- tagRule "image" "images, inline with text" $ do
        url   <- req Right "url"   "The URL of the image to embed."
        title <- opt Right "title" "The title/alt-text of the image."
        pure $ do
            H.img ! A.src (H.toValue url) ! foldMap (A.title . H.toValue) title

    bold <- tagRule "b" "bold text" $ do
        H.strong <$> children inline 

    italic <- tagRule "i" "italic text" $ do
        H.em <$> children inline

    literal <- tagRule "lit" "inline literal text" $ do
        H.code <$> children inline

    link <- tagRule "link" "hyperlinks" $ do
        url     <- req Right "url" "The destination URL."
        content <- children inline
        pure $ H.a content ! A.href (H.toValue url)

    tags "inline tags"
        [ bold
        , image
        , italic
        , literal
        , link
        ]

compileLiteralTag :: ProductT (Spanned LiteralTag) Identity H.Html
compileLiteralTag = do
    literalText <- rule "literal-text" "text inside of a literal tag" $ do
        content <- self
        pure $ H.text (content ^. _Literal)

    code <- tagRule "code" "embedded source code" $ do
        content <- child literalText
        pure $
            H.pre . H.code $ content

    tags "literal tags"
        [ code
        ]

compiler :: Bool -> Text -> ProductT Document Identity H.Html
compiler standalone space = mdo
    block <- blockRule "top-level block items"
        paragraph
        literalTag
        blockTag

    inline <- inlineRule "top-level inline items" 
        (pure " ")
        (pure . H.text)
        inlineTag

    paragraph  <- paragraphRule inline (pure . H.p)
    inlineTag  <- compileInlineTag inline
    blockTag   <- compileBlockTag paragraph block
    literalTag <- compileLiteralTag

    documentRule $ do
        content    <- children block
        title      <- req Right "title" "The document's title, used in the header."
        maybeStyle <- if 
          | standalone -> opt Right "style" "A stylesheet to attach to the document."
          | otherwise  -> pure Nothing
        pure $ do
            let titleHtml = H.text title
            if 
              | standalone -> do
                H.html $ do
                    H.head $ do
                        H.title titleHtml
                        for_ maybeStyle $ \style ->
                            H.link ! A.rel "stylesheet"
                                   ! A.type_ "text/css"
                                   ! A.href (H.toValue style)
                    H.body $ do
                        H.header $ H.h1 titleHtml
                        H.main content
              | otherwise -> do
                H.h1 titleHtml
                content 

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
