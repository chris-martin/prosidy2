{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
module Prosidy.Manual where

import           Text.Blaze.Html5               ( (!) )
import           Prosidy
import           Prosidy.Manual.Monad
import           Prosidy.Manual.Slug            ( slug )
import           Control.Lens.Operators

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
import           Control.Monad                  ( when
                                                , unless
                                                )
import qualified Data.Text                     as Text
import qualified Data.Char                     as Char
import qualified Data.Map.Strict               as Map
import           Numeric.Natural                ( Natural )

import           Debug.Trace

type Html a = C.Product a Manual H.Html

compile
    :: Html a
    -> FilePath
    -> TOC.TableOfContents
    -> a
    -> Either ManualError LBS.ByteString
compile h fp toc input = do
    result <- runManual (C.compileM h input) fp toc
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

    blockTagRule <- blockTag blockRule paragraphRule

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
        title    <- C.reqText "title" "This manual page's title."
        subtitle <- C.optText "subtitle" "This manual page's subtitle."
        revision <- C.optText "revision"
                              "The version of this page in the manual."
        created <- C.optText
            "created"
            "The date this manual page was initially created."
        updated <- C.optText "updated"
                             "The date this manual page was last updated."
        lang  <- C.optText "lang" "The language of this page."
        noToc <- C.prop "no-toc"
                        "Do not show the table of contents on this page."

        _ <- C.optText "toc-title"
                       "The title as it appears in the table of contents."
        _ <- C.prop
            "hidden"
            "Hide this document from the table of contents entirely."
        _ <- C.opt @Integer
            "priority"
            "The sort order in the table of contents. Higher goes first."

        body    <- C.children blockRule
        tocHtml <- C.embed $ do
            params <- parameters
            pure $ toc (currentPath params) (tableOfContents params)

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

blockTag
    :: C.ProductRule Block Manual H.Html
    -> C.ProductRule Paragraph Manual H.Html
    -> Html BlockTag
blockTag blockRule paragraphRule = do
    noteRule <-
        C.tag "note" "An aside, adding additional information to text." $ do
            level <- C.req @NoteLevel "level" "The 'severity' of the note."
            body  <- C.children blockRule
            pure $ H.aside body ! HA.class_ (H.toValue level)

    listRule    <- list blockRule

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
                let theSlug =
                        slug $ title `fromMaybe` tocTitle `fromMaybe` slugText
                H.section ! HA.id (H.toValue theSlug) $ do
                    hTag $ H.text title
                    body

    C.oneOf "block tag"
            "A top-level block tag."
            [noteRule, listRule, sectionRule]

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
                  rep  <- C.reqText "rep" "The literal sequence of characters."
                  body <- C.children textOnly
                  pure $ do
                      let escaped = Text.concatMap
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
                              for_ codepoints $ \point -> H.toHtml $ show point

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
        [boldRule, charsRule, defRule, italicRule, litRule, linkRule, termRule]

literalTag :: Html LiteralTag
literalTag = do
    C.oneOf "literal tag" "A top-level literal tag." []

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


toc :: FilePath -> TOC.TableOfContents -> H.Html
toc currentPath (TOC.TableOfContents entries) =
    H.ol . for_ (Map.toList entries) $ \(Slug.FileSlug _ path, entry) ->
        tocItem (Just currentPath) 0 path entry

tocItem :: Maybe FilePath -> Natural -> FilePath -> TOC.Entry -> H.Html
tocItem currentPath depth itemPath entry = do
    let uri | null currentPath = H.toValue itemPath <> "#" <> itemSlug
            | otherwise        = H.toValue itemPath
        itemAttributes | currentPath == Just itemPath = HA.class_ "current"
                       | otherwise                    = mempty
        title    = H.text $ TOC.entryTitle entry
        children = TOC.entryChildren entry
        itemSlug = H.toValue (TOC.entrySlug entry)
    H.li ! itemAttributes $ do
        H.a title
            ! HA.href uri
            ! (if null currentPath
                  then H.dataAttribute "target" itemSlug
                  else mempty
              )
        unless (depth > 1 || null children) . H.ol . for_ children $ tocItem
            Nothing
            (succ depth)
            itemPath

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
