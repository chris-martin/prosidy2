## vi: ft=prosidy wrap
title: Manual.lhs
created: 2020-01-01T17:22-8000
updated: 2020-01-01T17:22-8000

---

#=haskell:
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
#:

#=haskell[hide]:
module Prosidy.Manual
    ( compileDocument
    , compileToc
    , ManualError(..)
    ) where
#:

#=haskell:
import Prosidy
import Prosidy.Compile
import Prosidy.Manual.Monad
import Prosidy.Manual.TableOfContents

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

import qualified Hakyll as Ha (Item, Identifier, toFilePath, itemIdentifier, itemBody)

import qualified Control.Lens                as L
import qualified Data.Text                   as Text
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
#:

#=haskell:
type Html input = RuleT input Manual H.Html
#:

Our rules for parsing are all contained inside of the #lit{document} rule. Prosidy requires that a rule must be registered before it can be used to prevent some issues with non-termination.

Because rules are mutually recursive, we have to use #lit{MonadFix} for this to work! #lit{RecursiveDo} is a wonderful extension that provides from syntactic sugar for this.

#=haskell:
document :: [Ha.Item TocItem] -> Html Document
document toc = mdo
#:

Let's start with the non-recursive rules.

For text nodes, we should just convert their content directly into HTML. The #lit{self} descriptor returns the focus of a rule directly.

#=haskell:
    plainText <- rule @Text @Manual @H.Html "plain text" $
       fmap H.text self
#:

Prosidy inserts empty markers called #def{breaks} between lines in a paragraph and before or after an inline tag (if it has a space on that side). Because #i{CJKV} languages don't really care about spacing, we don't assume the user wants a space in these situations.

The Prosidy manual, however, is in English, which #i{does} want spaces in these locations. When we encounter a break, we should render a single space.

#=haskell:
    break <- rule "break" $
        pure $ H.text " "
#:

#=haskell:
    block <- choose "block context"
        [ _BlockParagraph @? paragraph
        , _BlockTag @? blockTag
        ]

    inline <- choose "inline context"
        [ _Break @? break
        , _InlineTag @? inlineTag
        , _InlineText @? plainText
        ]
#:

#=haskell:
    paragraph <- rule "paragraph" $ do
        body <- descend inline $ _Paragraph . L.folded
        pure $ H.p body
#:

#=haskell:
    blockTag <- choose "block tag"
        [ _Tagged "section" @? section
        , _Tagged "note" @? note
        ]

    inlineTag <- choose "inline tag"
        [ _Tagged "b" @? bold
        , _Tagged "i" @? italics
        , _Tagged "lit" @? inlineLiteral
        , _Tagged "def" @? definition
        , _Tagged "ref" @? reference
        , _Tagged "link" @? link
        ]
#:

With that out of the way, lets start defining custom elements for the manual.

#-section[title='Custom Markup']:

#=haskell:
    bold <- rule "boldface" $ do
        body <- descend inline $ content . L.folded
        pure $ H.strong body

    italics <- rule "italics" $ do
        body <- descend inline $ content . L.folded
        pure $ H.em body
#:

#=haskell:
    inlineLiteral <- rule "inline literal" $ do
        body <- descend inline $ content . L.folded
        pure $ H.code body
#:

#=haskell:
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
#:

#=haskell:
    link <- rule "link" $ do
      url <- reqText "url" "The URL to link to."
      external <- prop "external" "If provided, open links in a new window."
      body <- descend inline $ content . L.folded
      pure $ do
        H.a ! A.href (H.toValue url)
            ! (if external then A.target "blank" else mempty)
            $ body
#:

#=haskell:
    definition <- rule "term definition" $ do
        body <- descend inline $ content . L.folded
        pure $ H.dfn body

    reference <- rule "term reference" $ do
        body <- descend inline $ content . L.folded
        pure $ H.a body
#:

#=haskell:
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
            H.section $ do
                H.h1 title
                body
#:

#:

Finally, we wrap up all of the rules we previously defined into a final rule which processes the whole #lit{Document}:

#=haskell:
    rule "manual page" $ do
        title <- reqText "title" "The manual page's title."
        subtitle <- optText "subtitle" "The manual page's subtitle."
        _ <- optText "nav-title" "Use this title in the navigation instead of title."
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
                    H.title htmlTitle
                    H.link ! A.rel "stylesheet"
                           ! A.type_ "text/css"
                           ! A.href "https://fonts.googleapis.com/css?family=PT+Serif:400,400i,700,700i&display=swap"
                    H.link ! A.rel   "stylesheet"
                           ! A.type_ "text/css"
                           ! A.href  "res/manual.css"
                    for_ template $ \s ->
                      H.link ! A.rel   "stylesheet"
                             ! A.type_ "text/css"
                             ! A.href  (H.toValue $ "res/" <> s <> ".css")
                H.body $ do
                    when showToc . H.nav $ do
                        compileToc toc
                    H.header . H.hgroup $ do
                        H.h1 htmlTitle
                        for_ subtitle $ H.h2 . H.text
                    H.main body
                    H.footer "Copyright ©2020 to Prosidy.org.\
                             \ All rights reserved."
#:

#=haskell:
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
#:

#=haskell:
compileToc :: [Ha.Item TocItem] -> H.Html
compileToc = H.ol . foldMap compileTocItem

compileTocItem :: Ha.Item TocItem -> H.Html
compileTocItem item = H.li $ do
  let TocItem title slug children = Ha.itemBody item
      itemPath                    = Ha.toFilePath (Ha.itemIdentifier item) -<.> "html"
      itemUrl                     = Text.pack itemPath <> "#" <> slug
  H.a (H.text title) ! A.href (H.toValue itemUrl)
  unless (null children) $
    H.ol (foldMap compileTocItem $ fmap (item $>) children)
#:

#=haskell:
compileDocument :: [Ha.Item TocItem] -> Document -> Either ManualError H.Html
compileDocument toc doc =
    runManual (compileM (document toc) doc) >>= first CompileError

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
#:
