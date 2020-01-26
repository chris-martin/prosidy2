{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Prosidy.Manual.Compile (compile, document) where

import           Text.Blaze.Html5               ( (!) )
import           Prosidy
import           Prosidy.Manual.Monad
import           Prosidy.Manual.Slug            ( Slug(..), FileSlug(..), slug )
import           Control.Lens.Operators
import           System.FilePath ((-<.>), makeRelative, takeDirectory, takeBaseName)
import Control.Applicative ((<|>))

import qualified Prosidy.Manual.TableOfContents
                                               as TOC
import qualified Prosidy.Manual.Slug           as Slug
import           Data.Foldable                  ( for_ )

import           Data.Maybe                     ( fromMaybe )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as HA
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import qualified Prosidy.Compile               as C
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as Text
import qualified Data.Map.Strict               as Map
import           Numeric.Natural                ( Natural )
import Control.Monad (unless)
import qualified Data.Char as Char

type Html a = C.Product a Manual H.Html

compile
    :: Html a
    -> TOC.TableOfContents
    -> a
    -> Either ManualError LBS.ByteString
compile h contents input = do
    result <- runManual (C.compileM h input) contents
    case result of
        Left  err -> Left $ CompileError err
        Right ok  -> Right $ renderHtml ok

document :: Html Document
document = mdo
    blockRule <- C.block "block"
                         "Top-level block items."
                         blockTagRule
                         literalTagRule
                         paragraphRule

    blockTagRule <- blockTag blockRule

    inlineRule   <- C.inline "inline"
                             "Top-level inline items."
                             inlineTagRule
                             fragmentRule
                             breakRule

    inlineTagRule  <- inlineTag inlineRule

    literalTagRule <- literalTag

    paragraphRule  <-
        C.paragraph "paragraph" "Paragraph consisting of inline items." $ do
            body <- C.children inlineRule
            pure $ H.p body

    fragmentRule <- C.fragment "plain text"
                               "Plain, unmodified textual content."
                               (pure . H.text)

    breakRule <- C.break "break" "Single spaces between lines." (pure " ")

    C.document $ do
        title     <- C.reqText "title"    "This manual page's title."
        subtitle  <- C.optText "subtitle" "This manual page's subtitle."
        lang      <- C.optText "lang"     "The language of this page."
        _revision <- C.optText "revision" "The version of this page in the manual."
        _created  <- C.optText "created"  "The date this manual page was initially created."
        _updated  <- C.optText "updated"  "The date this manual page was last updated."
        _noToc    <- C.prop    "no-toc"   "Do not show the table of contents on this page."

        -- These options are used in the table-of-contents preprocessor,
        -- but specifying them ensures that they're properly validated.
        _ <- C.optText      "toc-title" "The title as it appears in the table of contents."
        _ <- C.prop         "hidden"    "Hide this document from the table of contents entirely."
        _ <- C.opt @Integer "priority"  "The sort order in the table of contents. Higher goes first."

        body    <- C.children blockRule
        tocHtml <- C.embed $ do
            params <- parameters
            pure $ toc (tableOfContents params)

        pure $ do
            let titleText = H.text title
            H.html ! HA.lang (foldMap H.toValue lang) $ do
                H.head $ do
                    H.meta ! HA.charset "UTF-8"
                    H.title $ H.text title
                    style "res/manual.css"
                    H.script mempty ! HA.src "res/manual.js"
                H.body $ do
                    H.header $ do
                        H.h1 titleText
                        foldMap (H.h2 . H.text) subtitle
                    H.nav tocHtml
                    H.main body
                    H.footer $ do
                        H.div "Copyright ©2020 James Alexander Feldman-Crough" ! HA.id "copyright"

blockTag
    :: C.ProductRule Block Manual H.Html
    -> Html BlockTag
blockTag blockRule = do
    listRule <- list blockRule

    noteRule <-
        C.tag "note" "An aside, adding additional information to text." $ do
            level <- C.req @NoteLevel "level" "The 'severity' of the note."
            body  <- C.children blockRule
            pure $ H.aside body ! HA.class_ (H.toValue level)

    specRule <-
        C.tag "spec" "A part of the specification with test cases." $ do
            _test <- C.reqText "test" "The name of the test."
            body  <- C.children blockRule
            pure body

    sectionRule <-
        C.tag "section" "A container which deliniates sections of a page." $ do
            title    <- C.reqText "title" "The title of the section."
            tocTitle <- C.optText
                "toc-title"
                "An optional alternative name to show in the table of contents."
            slugText <- C.optText
                "slug"
                "An optional identifier to use as an anchor to this section."
            body <- C.children $ C.hoist nesting blockRule
            hTag <- C.embed headerTag
            pure $ do
                let theSlug = slug 0 $ title `fromMaybe` tocTitle `fromMaybe` slugText
                H.section ! HA.id (H.toValue theSlug) $ do
                    hTag $ H.text title
                    body

    C.oneOf "block tag"
            "A top-level block tag."
            [noteRule, listRule, sectionRule, specRule]

inlineTag :: C.ProductRule Inline Manual H.Html -> Html InlineTag
inlineTag inlineRule = do
    textRule <- C.fragment "semantic text" "Plain text, used semantically." pure

    textOnly <- C.inline
        "text-only"
        "Text only inline context."
        (C.disallow "Only text is allowed in this context.")
        textRule
        (C.disallow "Please include all text on a single line in this context.")

    boldRule <- C.tag "b" "Boldface text." $ do
        H.strong <$> C.children inlineRule

    charsRule <-
        C.tag
                "chars"
                "Displays a sequence of characters alongside its Unicode hexadecimal \
        \representation."
            $ do
                  rep      <- C.reqText "rep" "The literal sequence of characters."
                  noEscape <- C.prop "no-escape" "Do not escape special characters."
                  body     <- C.children textOnly
                  pure $ do
                      let escaped | noEscape  = rep
                                  | otherwise = Text.concatMap
                                    (\c -> Text.pack $ Char.showLitChar c [])
                                    rep
                          codepoints = Char.ord <$> Text.unpack rep
                      H.text body
                      " "
                      H.span ! HA.class_ "character-sequence" $ do
                          H.code (H.text escaped) ! HA.class_ "display"
                          " "
                          H.code ! HA.class_ "codepoints" $ do
                              "0x"
                              for_ codepoints $ \point -> 
                                  H.span (H.toHtml $ hexadecimalize point) 
                                    ! HA.class_ "word"

    defRule <- C.tag "def" "Defines a term." $ do
        _lemma        <- C.optText "lemma" "The dictionary-form of the term."
        body          <- C.children textOnly
        (slug, lemma) <- C.access $ \rawBody -> do
            let providedLemma = rawBody ^. setting "lemma"
                bodyText =
                    rawBody ^. content . traverse . _InlineFragment . content
                lemma = fromMaybe bodyText providedLemma
            slug <- define lemma
            pure (slug, lemma)
        pure $ H.dfn (H.toHtml body) ! HA.title (H.toValue lemma) ! HA.id
            ("term-" <> H.toValue slug)

    italicRule <- C.tag "i" "Italicized text." $ do
        H.em <$> C.children inlineRule

    linkRule <- C.tag "link" "A link to a page outside of the manual." $ do
        url      <- C.reqText "url" "The URL that the hyperlink directs toward."
        samePage <- C.prop
            "same-page"
            "If specified, open the hyperlink in the same tab/window."
        body <- C.children inlineRule
        pure $ H.a body ! HA.href (H.toValue url) ! if samePage
            then mempty
            else HA.target "_blank"

    litRule <- C.tag "lit"
                     "Literal text, inline with normal text."
                     (H.code <$> C.children inlineRule)

    refRule <- C.tag "ref" "References a section in the same document." $ do
        section <- C.reqText "section" "The title of the section to link to."
        body <- C.children inlineRule
        pure $ H.a body ! HA.href ("#" <> H.toValue (slug 0 section))

    termRule <-
        C.tag "term"
              "References a term defined elsewhere in the body with `def`."
            $ do
                  _lemma <- C.optText "lemma" "The dictionary-form of the term."
                  body   <- C.children textOnly
                  slug   <- C.access $ \rawBody -> do
                      let providedLemma = rawBody ^. setting "lemma"
                          bodyText =
                              rawBody
                                  ^. content
                                  .  traverse
                                  .  _InlineFragment
                                  .  content
                          lemma = fromMaybe bodyText providedLemma
                      reference lemma
                  pure
                      $ H.a (H.toHtml body)
                      ! HA.class_ "term-reference"
                      ! HA.href ("#" <> "term-" <> H.toValue slug)

    C.oneOf
        "inline tag"
        "A top-level inline tag."
        [boldRule, charsRule, defRule, italicRule, litRule, linkRule, refRule, termRule]

hexadecimalize :: Int -> String
hexadecimalize = go []
  where
    go []  0 = "0"
    go acc 0 = acc
    go acc n
      | n >= 16   = let (q, r) = quotRem n 16 in go (go acc r) q
      | n >= 10   = Char.chr (Char.ord 'A' + n - 10) : acc
      | otherwise = show n ++ acc

literalTag :: Html LiteralTag
literalTag = do
    codeRule <- C.literal "source code" "Source code literal" 
        (pure . H.text)

    srcRule <-
        C.tag "src" "A block of source code." $ do
            lang <- C.optText "lang" "The programming language of the source-code block."
            body <- C.child codeRule
            pure $ H.pre 
                ! HA.class_ "source-code" 
                ! foldMap (H.dataAttribute "language" . H.toValue) lang
                $ H.code body

    C.oneOf "literal tag" "A top-level literal tag." 
        [ srcRule
        ]

list :: C.ProductRule Block Manual H.Html -> C.Sum BlockTag Manual H.Html
list blockRule = do
    itemRule <- C.tag "item"
                      "A single item in a list"
                      (H.li <$> C.children blockRule)

    listBlockTagRule <- C.oneOf
        "list block"
        "A block tag which fails on anything that is not an `item`."
        [itemRule]

    listBlockRule <- C.block
        "list content"
        "Inner content of the `list` tag. Only contains `item`s."
        listBlockTagRule
        (C.disallow "Literals are not allowed as children of a list.")
        (C.disallow "Paragraphs are not allowed as children of a list.")

    C.tag "list" "A list of items." $ do
        ordered <- C.prop "ord" "If provided, the list is an ordered list."
        content <- C.children listBlockRule
        pure $ (if ordered then H.ol else H.ul) content


toc :: TOC.TableOfContents -> H.Html
toc theToc = H.ol $ do
    foldMap (uncurry tocOther) before 
    tocCurrent thisSlug Nothing thisContents
    foldMap (uncurry tocOther) after 
  where
    (before, (thisSlug, thisContents), after) = TOC.orderedEntries theToc

tocCurrent :: FileSlug -> Maybe Slug -> TOC.TOCEntry -> H.Html
tocCurrent fp slug tocEntry = H.li ! HA.class_ "current" $ do
    H.a (H.text $ TOC.entryTitle tocEntry) 
        ! foldMap (H.dataAttribute "target" . H.toValue) slug
        ! HA.href (H.toValue (fileSlugPath fp) <> foldMap (("#" <>) . H.toValue) slug)
    H.ol . for_ (TOC.toList $ TOC.entryChildren tocEntry) $ 
        \(slug, children) -> tocCurrent fp (Just slug) children

tocOther :: FileSlug -> Metadata -> H.Html
tocOther fp md 
  | md ^. property "hidden" = mempty
  | otherwise               = H.li $ H.a title ! HA.href (H.toValue path)
    where
      path  = fileSlugPath fp
      title = H.text . fromMaybe "Untitled" $ 
        (md ^. setting "toc-title") <|> (md ^. setting "title")
    
-------------------------------------------------------------------------------
style :: H.AttributeValue -> H.Html
style uri = H.link ! HA.href uri ! HA.rel "stylesheet" ! HA.type_ "text/css"

-------------------------------------------------------------------------------
data NoteLevel =
    Info
  | Tip
  | Important
  | Caution
  deriving (Show, Eq, Ord, Enum)

instance H.ToValue NoteLevel where
    toValue Info      = H.textValue "info"
    toValue Tip       = H.textValue "tip"
    toValue Important = H.textValue "important"
    toValue Caution   = H.textValue "caution"

instance Read NoteLevel where
    readsPrec _ s = case Text.toLower . Text.strip $ Text.pack s of
        "info"      -> [(Info, "")]
        "tip"       -> [(Tip, "")]
        "important" -> [(Important, "")]
        "caution"   -> [(Caution, "")]
        _           -> []
