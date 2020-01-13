{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Prosidy.Manual
    ( compileDocument
    , compileToc
    , ManualError(..)
    ) where
import Prosidy
import Prosidy.Compile
import Prosidy.Manual.Monad
import Prosidy.Manual.TableOfContents

import Data.Tuple (swap)
import Data.Functor (($>))
import Data.Bifunctor      (Bifunctor(..))
import Data.Text           (Text)
import Data.Sequence       (Seq)
import Data.Foldable       (for_, traverse_)
import Text.Blaze.Html5    ((!))
import Text.Read           (readEither)
import Type.Reflection     (Typeable)
import Control.Monad (unless, when)
import Control.Monad.Morph (MFunctor(hoist))
import Control.Lens.Operators
import Data.Traversable (sequenceA)
import System.FilePath((-<.>))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)

import qualified Hakyll as Ha (Item, Identifier, toFilePath, itemIdentifier, itemBody)

import qualified Control.Lens                as L
import qualified Data.Text                   as Text
import qualified Data.ByteString             as BS
import qualified Data.Char                   as Char
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

type Html input = RuleT input Manual H.Html

document :: FilePath -> [Ha.Item TocItem] -> Html Document
document currentPath toc = mdo
    plainTextText <- rule "plain text (as text)" $ self
    plainText     <- rule "plain text" $ fmap H.text self
    breakText <- rule "break (text)" $ pure (" " :: Text)
    break     <- rule "break" $ pure $ H.text " "
    block <- choose "block context"
        [ _BlockParagraph . spanning @? paragraph
        , _BlockTag . spanning @? blockTag
        ]

    inline <- choose "inline context"
        [ _Break @? break
        , _InlineTag . spanning @? inlineTag
        , _InlineText . spanning @? plainText
        ]

    onlyText <- choose "inline context (only text)"
        [ _Break @? breakText
        , _InlineText . spanning  @? plainTextText
        ]

    paragraph <- rule "paragraph" $ do
        body <- descend inline $ _Paragraph . L.folded
        pure $ H.p body

    blockTag <- choose "block tag"
        [ _Tagged "section" @? section
        , _Tagged "note" @? note
        , _Tagged "list" @? list
        ]

    inlineTag <- choose "inline tag"
        [ _Tagged "b" @? bold
        , _Tagged "i" @? italics
        , _Tagged "chars" @? chars
        , _Tagged "lit" @? inlineLiteral
        , _Tagged "def" @? definition
        , _Tagged "term" @? reference
        , _Tagged "link" @? link
        ]

    bold <- rule "boldface" $ do
        body <- descend inline $ content . L.folded
        pure $ H.strong body

    italics <- rule "italics" $ do
        body <- descend inline $ content . L.folded
        pure $ H.em body

    inlineLiteral <- rule "inline literal" $ do
        body <- descend inline $ content . L.folded
        pure $ H.code body

    listItemBody <- rule "list item body" $ do
        body <- descend block $ content . L.folded
        pure $ H.li body

    listItem <- choose "list item" 
        [ _BlockTag . spanning . _Tagged "item" @? listItemBody
        ]

    list <- rule "list" $ do
        body <- descend listItem $ content . L.folded
        pure $ H.ul body

    chars <- rule "chars" $ do
        noEscape <- prop "no-escape" "Do not escape the `rep` setting."
        rep      <- reqText "rep" "The literal representation of the inner text."
        text     <- descend onlyText $ content . L.folded
        pure $ do
            let pickHex  = (!!) "0123456789ABCDEF" . fromIntegral
                toHex 0 acc = acc
                toHex n acc = uncurry toHex
                    . second ((: acc) . pickHex)
                    $ n `quotRem` 16
            H.span ! A.class_ "char-sequence" $ do
                H.text text
                " ("
                H.code ! A.class_ "char-sequence-literal" $
                    foldMap (H.span . H.toHtml) $ 
                        if noEscape 
                        then Text.unpack rep
                        else foldMap Char.showLitChar (Text.unpack rep) ""
                "; "
                H.code ! A.class_ "char-sequence-utf-8" $ do
                    for_ (Text.unpack rep) $ \ch -> H.span $
                        H.toHtml . foldr toHex [] . BS.unpack . encodeUtf8 $ 
                            Text.singleton ch
                ")"
    note <- rule "note" $ do
      level <- req readNote "level"
        "The level of the note. Can be 'caution', 'note', or 'wip'"
      body <- descend block $ content . L.folded
      pure $ do
        let levelHtml = case level of
              Caution -> "caution"
              Note    -> "note"
              WIP     -> "wip"
        H.aside ! A.class_ levelHtml $ do
          body

    link <- rule "link" $ do
      url <- reqText "url" "The URL to link to."
      external <- prop "external" "If provided, open links in a new window."
      body <- descend inline $ content . L.folded
      pure $ do
        H.a ! A.href (H.toValue url)
            ! (if external then A.target "blank" else mempty)
            $ body

    definition <- rule "term definition" $ do
        body <- descend onlyText $ content . L.folded
        lemma <- optText "lemma" 
            "The optional, canonical form of a definition\
            \ used when the term being defined is declined to fit the sentence."
        pure $ do
            let id = H.toValue . ("term-" <>) . toSlug $ fromMaybe body lemma
            H.dfn (H.text body) ! A.id id

    reference <- rule "term reference" $ do
        body <- descend onlyText $ content . L.folded
        lemma <- optText "lemma" 
            "The optional, canonical form of a definition\
            \ used when the term being defined is declined to fit the sentence."
        pure $ do
            let id = H.toValue . ("term-" <>) . toSlug $ fromMaybe body lemma
            H.a (H.text body) ! A.href ("#" <> id) ! A.class_ "term-reference"

    section <- rule "section" $ do
        sTitle <- reqText "title" "The title of the section."
        sSlug  <- optText "slug" "The anchor tag associated with the section.\
            \ If not provided, one will be generated based on the title.\
            \ Generally, providing a slug is better as it allows the section's\
            \ name to be changed without breaking permalinks."

        hTag <- embed headerTag

        body  <- descend (hoist nestSection block) $
            content . L.folded

        pure $ do
            let title = H.text sTitle
            let slug  = fromMaybe (toSlug sTitle) sSlug
            H.section ! A.id (H.toValue slug) $ do
                H.h1 title
                body
    rule "manual page" $ do
        title <- reqText "title" "The manual page's title."
        subtitle <- optText "subtitle" "The manual page's subtitle."
        _ <- optText "nav-title" "Use this title in the navigation instead of title."
        _ <- prop "hide" "Do not include in the table-of-contents if specified."
        template <- optText "template"
            "An optional name of a template to use.\
            \ Activating a template will include extra CSS on the page,\
            \ and in the future it may also modify the ruleset used\
            \ to compile the document itself."
        showToc <- prop "toc" "If provided, show the TOC on this page."
        body  <- descend block $ content . L.folded
        pure $ do
            let htmlTitle = H.text title
            H.html $ do
                H.head $ do
                    H.meta ! A.charset "UTF-8"
                    H.meta ! A.name "viewport"
                           ! A.content "width=device-width, initial-scale=1"
                    H.title htmlTitle
                    H.link ! A.rel   "stylesheet"
                           ! A.type_ "text/css"
                           ! A.href  "res/manual.css"
                    H.script mempty ! A.src "res/manual.js"
                    for_ template $ \s ->
                      H.link ! A.rel   "stylesheet"
                             ! A.type_ "text/css"
                             ! A.href  (H.toValue $ "res/" <> s <> ".css")
                H.body ! (if showToc then A.class_ "toc" else mempty) $ do
                    H.header . H.hgroup $ do
                        H.h1 htmlTitle
                        for_ subtitle $ H.h2 . H.text
                    when showToc . H.nav $ do
                        compileToc currentPath toc
                    H.main body
                    H.footer $ do
                        H.div ! A.id "copyright" $ do
                          "Copyright ©2020 to Prosidy.org. Available under the "
                          H.a "MPL v2.0" ! A.href "https://www.mozilla.org/en-US/MPL/2.0/"
                          " license."
data NoteLevel = Caution | Note | WIP
  deriving Show

readNote :: Text -> Either String NoteLevel
readNote "caution" = Right Caution
readNote "note"    = Right Note
readNote "wip"     = Right WIP
readNote unknown   = Left $ mconcat
  [ "Unknown note type: "
  , show unknown
  , ". Expected 'caution', 'note', or 'wip'"
  ]
compileToc :: FilePath -> [Ha.Item TocItem] -> H.Html
compileToc currentPath = H.ol . foldMap (compileTocItem $ Just currentPath)

compileTocItem :: Maybe FilePath -> Ha.Item TocItem -> H.Html
compileTocItem currentPath item =
    H.li ! (if isCurrent then A.class_ "current" else mempty) $ do
        H.a (H.text title) ! A.href (H.toValue itemUrl)
                           ! (if isCurrent then mempty else H.dataAttribute "target" (H.toValue slug))
        unless (null children) $
            H.ol (foldMap (compileTocItem Nothing) $ 
                fmap (item $>) children)
  where
    TocItem title slug children = Ha.itemBody item
    itemPath                    = Ha.toFilePath $ Ha.itemIdentifier item
    itemURI                     = itemPath -<.> ".html"
    itemUrl                     = Text.pack itemURI <> "#" <> slug
    isCurrent                   = Just itemPath == currentPath
compileDocument :: [Ha.Item TocItem] -> Ha.Item Document -> Either ManualError H.Html
compileDocument toc item =
    runManual (compileM (document currentPath toc) doc) >>= first CompileError
  where
    currentPath = Ha.toFilePath $ Ha.itemIdentifier item
    doc         = Ha.itemBody item

reqAuto
    :: (Typeable output, Read output, HasMetadata input, Monad context)
    => Key -> Text -> Desc input context output
reqAuto = req $ \inputText ->
  let
    input        = Text.unpack inputText
    annotate msg = msg <> "(input: " <> show inputText <> ")"
  in
    first annotate $ readEither input

reqText :: (HasMetadata input, Monad context) => Key -> Text -> Desc input context Text
reqText = req Right

optText :: (HasMetadata input, Monad context) => Key -> Text -> Desc input context (Maybe Text)
optText = opt Right
