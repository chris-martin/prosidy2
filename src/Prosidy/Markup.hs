{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Prosidy.Markup
    ( main
    )
where

import           Prosidy
import qualified Prosidy.Compile               as C

import           Data.Maybe                     ( fromMaybe )
import           Options.Applicative
import           Data.Text                      ( Text )
import           Control.Exception              ( throwIO
                                                , bracket
                                                )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import           Data.ByteString.Lazy                 ( toStrict )
import           Text.Blaze.Html5               ( (!) )

import qualified System.IO                     as IO
import           Data.Foldable                  ( for_ )
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)

import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Control.Monad.Trans.Reader     ( Reader
                                                , runReader
                                                , asks
                                                )
import           System.Exit                    ( exitFailure )
import           Control.Exception              ( displayException )

main :: IO ()
main = do
    Opts {..} <- getOpts
    input <- withFile' IO.stdin inputFile IO.ReadMode (fmap decodeUtf8 . BS.hGetContents)
    document <- either throwIO pure
        $ parseDocument (fromMaybe "<stdin>" inputFile) input
    case
            runReader (C.compileM documentC document)
                      (ManualState standalone breakUsing)
        of
            Left err -> do
                IO.hPutStrLn IO.stderr $ displayException err
                exitFailure
            Right html -> do
                withFile' IO.stdout outputFile IO.WriteMode $ \handle -> do
                    BS.hPut handle . toStrict . renderHtml $ html
                    BS.hPut handle "\n"

withFile'
    :: IO.Handle -> Maybe FilePath -> IO.IOMode -> (IO.Handle -> IO a) -> IO a
withFile' hdl path mode =
    bracket (maybe (pure hdl) (`IO.openFile` mode) path) IO.hClose

type Product i = C.Product i (Reader ManualState) H.Html
type Sum i = C.Sum i (Reader ManualState) H.Html

data ManualState = ManualState Bool Text

isStandalone :: Reader ManualState Bool
isStandalone = asks $ \(ManualState x _) -> x

breakSeparator :: Reader ManualState Text
breakSeparator = asks $ \(ManualState _ x) -> x

documentC :: Product Document
documentC = mdo
    blockTag   <- blockTagC block paragraph
    inlineTag  <- inlineTagC inline
    literalTag <- literalTagC

    block      <- C.block "block context"
                          "Top level block items."
                          blockTag
                          literalTag
                          paragraph

    inline <- C.inline "inline context" "Primary content." inlineTag text break

    paragraph <- C.paragraph
        "paragraph"
        "Primary content, grouped by paragraph."
        do
            content <- C.children inline
            pure $ H.p content

    break <- C.break "space" "One-character space." (H.text <$> breakSeparator)

    text  <- C.fragment "text" "Textual data." (pure . H.text)

    C.document do
        standalone <- C.embed isStandalone
        title      <- C.reqText
            "title"
            "The document's title; inserted as the first header in a document."
        style <- C.optText
            "style"
            "An optional stylesheet attached to the document.\
            \ Ignored when not generating a standalone document."
        content <- C.children block
        pure if
            | standalone
            -> H.html do
                H.head $ do
                    H.title $ H.text title
                    for_ style $ \url ->
                        H.link
                            ! A.rel "stylesheet"
                            ! A.type_ "text/css"
                            ! A.href (H.toValue url)
                    H.body $ do
                        H.header . H.h1 $ H.text title
                        H.main content
            | otherwise
            -> do
                H.h1 $ H.text title
                content

blockTagC
    :: C.ProductRule Block (Reader ManualState) H.Html
    -> C.ProductRule Paragraph (Reader ManualState) H.Html
    -> Product (BlockTag)
blockTagC blockRule paragraphRule = do
    list     <- listC blockRule

    textOnly <- C.block
        "text-only"
        "A block which can only contain inline items."
        (C.disallow "Block tags are not allowed in a text-only tag.")
        (C.disallow "Literal tags are not allowed in a text-only tag.")
        paragraphRule

    heading1 <- C.tag "h" "top-level heading" (H.h2 <$> C.children textOnly)

    heading2 <- C.tag "h+" "second-level heading" (H.h3 <$> C.children textOnly)

    heading3 <- C.tag "h++" "third-level heading" (H.h4 <$> C.children textOnly)

    image    <- C.tag
        "image"
        "An image containing a capture, outside of the flow of text."
        do
            imgHtml <- imageDesc
            content <- C.children textOnly
            pure $ H.figure do
                imgHtml
                H.figcaption content

    quote <- C.tag
        "quote"
        "A block quotation."
        do
            content <- C.children blockRule
            pure $ H.blockquote content

    C.oneOf "block tag"
            "Top-level block tags."
            [heading1, heading2, heading3, image, list, quote]

inlineTagC
    :: C.ProductRule Inline (Reader ManualState) H.Html -> Product (InlineTag)
inlineTagC inlineRule = do
    bold <- C.tag
        "b"
        "Bold text."
        do
            content <- C.children inlineRule
            pure $ H.strong content

    image  <- C.tag "image" "An inline image." imageDesc

    italic <- C.tag
        "i"
        "Italic text."
        do
            content <- C.children inlineRule
            pure $ H.em content

    link <- C.tag
        "link"
        "Hyperlink to an external document."
        do
            url   <- C.reqText "url" "The destination URL of the hyperlink."
            title <- C.optText
                "title"
                "The link's title. Shown on mouseover in most browsers."
            content <- C.children inlineRule
            pure
                $ H.a content
                ! A.href (H.toValue url)
                ! foldMap (A.title . H.toValue) title

    lit <- C.tag
        "lit"
        "Literal text."
        do
            content <- C.children inlineRule
            pure $ H.code content

    C.oneOf "inline tag"
            "Top-level inline tags."
            [bold, image, italic, link, lit]

literalTagC :: Product (LiteralTag)
literalTagC = do
    literal <- C.literal "code literal"
                         "Embedded source code."
                         (pure . H.code . H.text)

    src <- C.tag
        "src"
        "Embedded source code. "
        do
            lang    <- C.optText "lang" "The language of the source code."
            content <- C.child literal
            pure do
                H.pre do
                    let attrs = foldMap (A.class_ . H.toValue) lang
                    H.code content ! attrs

    C.oneOf "literal tag" "Top-level literal tags." [src]

listC :: C.ProductRule Block (Reader ManualState) H.Html -> Sum (BlockTag)
listC blockRule = do
    item <- C.tag
        "item"
        "A single item in a list."
        do
            content <- C.children blockRule
            pure $ H.li content

    listTag <- C.oneOf
        "list block tag"
        "Block tags allowed inside of a list. Hint: it's only item."
        [item]

    listBlock <- C.block
        "list block"
        "A special block-context for the inside of lists."
        listTag
        (C.disallow "Literals are not allowed inside of lists.")
        (C.disallow "Paragraphs are not allowed inside of lists.")

    C.tag
        "list"
        "A list of items."
        do
            isOrdered <- C.prop
                "ord"
                "If provided, the items in the list are ordered."
            content <- C.children listBlock
            pure $ (if isOrdered then H.ol else H.ul) content

imageDesc :: (HasMetadata input, Monad context) => C.Desc input context H.Html
imageDesc = do
    url  <- C.reqText "url" "The URL of the image to display."
    desc <- C.optText
        "desc"
        "A description of the image; shown when the image cannot be displayed."
    title <- C.optText
        "title"
        "The image's title. Shown on mouseover in most browsers."
    pure
        $ H.img
        ! A.href (H.toValue url)
        ! foldMap (A.alt . H.toValue)   desc
        ! foldMap (A.title . H.toValue) title

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

