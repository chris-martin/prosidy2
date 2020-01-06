{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Prosidy.Types
    ( Block(..)
    , Document(..)
    , Inline(..)
    , Key
    , pattern Key
    , keyName
    , keyQ
    , toKey
    , toKeyUnchecked
    , isValidKeyHead
    , isValidKeyTail
    , Literal(..)
    , Metadata(..)
    , Paragraph(..)
    , Tagged(..)
    , tag
    , _Tagged
    , Region(..)
    , addTag
    , HasContent(..)
    , HasMetadata(..)
    , properties
    , property
    , settings
    , setting
    , NonEmpty
    , nonEmpty
    , getNonEmpty
    , BlockTag
    , InlineTag
    , LiteralTag
    , _BlockLiteral
    , _BlockTag
    , _BlockParagraph
    , _InlineTag
    , _InlineText
    , _Break
    , _Paragraph
    , _Literal
    ) where

import Prosidy.Internal.Optics
import Prosidy.Internal.JSON

import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..))
import Data.Aeson.Types (FromJSONKeyFunction(..))
import Data.String (IsString(..))
import Type.Reflection (Typeable)

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Q

-------------------------------------------------------------------------------
data Block where
    BlockTag       :: BlockTag   -> Block
    BlockParagraph :: Paragraph  -> Block
    BlockLiteral   :: LiteralTag -> Block
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (JSON Block)

instance Serde Block where
    serde = asum
        [ choice "tag" (Just "literal") _BlockLiteral
        , choice "paragraph" Nothing _BlockParagraph
        , choice "tag" (Just "block") _BlockTag
        , noMatch "Expected a LiteralTag, BlockTag, or Paragraph"
        ]

type BlockTag = Tagged (Seq Block)

type LiteralTag = Tagged Literal

_BlockLiteral :: Prism' Block LiteralTag
_BlockLiteral = prism BlockLiteral $ \case
    BlockLiteral item -> Just item
    _                 -> Nothing
{-# INLINE _BlockLiteral #-}

_BlockParagraph :: Prism' Block Paragraph
_BlockParagraph = prism BlockParagraph $ \case
    BlockParagraph item -> Just item
    _                   -> Nothing
{-# INLINE _BlockParagraph #-}

_BlockTag :: Prism' Block BlockTag
_BlockTag = prism BlockTag $ \case
    BlockTag item -> Just item
    _             -> Nothing
{-# INLINE _BlockTag #-}

-------------------------------------------------------------------------------
data Document where
    Document :: Metadata -> Seq Block -> Document
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via (JSON Document)

instance Serde Document where
    serde = Document
        <$> field "metadata" metadata
        <*> field "content"  content

instance HasContent Document where
    type Content Document = Seq Block
    content = lens get set
      where
        get (Document _ c)   = c
        set (Document m _) c = Document m c
    {-# INLINE content #-}

instance HasMetadata Document where
    metadata = lens get set
      where
        get (Document m _)   = m
        set (Document _ c) m = Document m c
    {-# INLINE metadata #-}

-------------------------------------------------------------------------------
data Inline where
    Break       ::              Inline
    InlineTag   :: InlineTag -> Inline
    InlineText  :: Text      -> Inline
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON) via (JSON Inline)

instance Serde Inline where
    serde = asum
        [ choice "break" Nothing _Break
        , choice "tag" (Just "inline") _InlineTag
        , choice "text" Nothing _InlineText
        , noMatch "Expected an Break, InlineTag, or Inline Text"
        ]

type InlineTag = Tagged (Seq Inline)

_Break :: Prism' Inline ()
_Break = prism (const Break) $ \case
    Break -> Just ()
    _     -> Nothing
{-# INLINE _Break #-}

_InlineTag :: Prism' Inline InlineTag
_InlineTag = prism InlineTag $ \case
    InlineTag t -> Just t
    _           -> Nothing
{-# INLINE _InlineTag #-}

_InlineText :: Prism' Inline Text
_InlineText = prism InlineText $ \case
    InlineText text -> Just text
    _               -> Nothing
{-# INLINE _InlineText #-}

-------------------------------------------------------------------------------
newtype Key where
    UncheckedKey :: Text -> Key
  deriving newtype (Eq, Hashable, Ord, Show, ToJSON, ToJSONKey)

instance FromJSON Key where
    parseJSON json = do
        text <- parseJSON json
        maybe (fail "Invalid character in Key") pure $
            toKey text

instance FromJSONKey Key where
    fromJSONKey = FromJSONKeyTextParser $ \text ->
        maybe (fail "Invalid character in Key") pure $
            toKey text

instance IsString Key where
    fromString k =
        fromMaybe (error $ "Invalid key: " <> k) .toKey $ Text.pack k

pattern Key :: Text -> Key
pattern Key text <- (keyName -> text)

keyQ :: Q.QuasiQuoter
keyQ = Q.QuasiQuoter
    { Q.quoteExp = checkKey $ \str ->
        [| UncheckedKey (Text.strip $ Text.pack str) |]
    , Q.quotePat = unsupported "patterns"
    , Q.quoteType = unsupported "types"
    , Q.quoteDec = unsupported "declarations"
    }
  where
    checkKey :: (String -> TH.Q t) -> String -> TH.Q t
    checkKey ok str@(c : cs)
        | not (isValidKeyHead c)      = fail $ "invalid key: " <> show str
        | not (all isValidKeyTail cs) = fail $ "invalid key: " <> show str
        | otherwise                   = ok str
    checkKey _ [] = fail "empty string cannot be used as a key"
    unsupported kind = fail $ kind <> " are not supported by the 'key' quasiquoter."

toKey :: Text -> Maybe Key
toKey text = do
    (keyHead, keyTail) <- Text.uncons text
    guard $
        isValidKeyHead keyHead &&
        Text.all isValidKeyTail keyTail
    Just $ UncheckedKey text

keyName :: Key -> Text
keyName (UncheckedKey key) = key

toKeyUnchecked :: Text -> Key
toKeyUnchecked = UncheckedKey

isValidKeyHead :: Char -> Bool
isValidKeyHead = (||) <$> Char.isAlphaNum <*> (== '_')

isValidKeyTail :: Char -> Bool
isValidKeyTail = not . invalid
  where
    invalid = (||) <$> Char.isSpace <*> (`Set.member` reserved)
    reserved = Set.fromList "\\#{}[]:=,"

-------------------------------------------------------------------------------
newtype Literal where
    Literal :: Text -> Literal
  deriving newtype (Eq, FromJSON, Show, ToJSON)

_Literal :: Iso' Literal Text
_Literal = iso (\(Literal txt) -> txt) Literal
{-# INLINE _Literal #-}

-------------------------------------------------------------------------------
data Metadata where
    Metadata :: Set Key -> Map Key Text -> Metadata
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (JSON Metadata)

instance HasMetadata Metadata where
    metadata = id
    {-# INLINE metadata #-}

instance Monoid Metadata where
    mempty = Metadata mempty mempty

instance Semigroup Metadata where
    Metadata x1 x2 <> Metadata y1 y2 =
        Metadata (x1 <> y1) (x2 <> y2)

instance Serde Metadata where
    serde = Metadata
        <$> field "properties" properties
        <*> field "settings"   settings

-------------------------------------------------------------------------------
newtype Paragraph where
    Paragraph :: NonEmpty Seq Inline -> Paragraph
  deriving newtype (Eq, FromJSON, Show, ToJSON)

_Paragraph :: Iso' Paragraph (NonEmpty Seq Inline)
_Paragraph = iso (\(Paragraph xs) -> xs) Paragraph
{-# INLINE _Paragraph #-}

-------------------------------------------------------------------------------
data Tagged content where
    Tagged :: Key -> Metadata -> content -> Tagged content
  deriving stock (Eq, Foldable, Functor, Show, Traversable)
  deriving (FromJSON, ToJSON) via (JSON (Tagged content))

instance HasContent (Tagged content) where
    type Content (Tagged content) = content
    content = lens get set
      where
        get (Tagged _ _ c)   = c
        set (Tagged k m _) c = Tagged k m c
    {-# INLINE content #-}

instance HasMetadata (Tagged c) where
    metadata = lens get set
      where
        get (Tagged _ m _)   = m
        set (Tagged k _ c) m = Tagged k m c
    {-# INLINE metadata #-}

instance (Typeable c, ToJSON c, FromJSON c) => Serde (Tagged c) where
    serde = Tagged
        <$> field "name" tag
        <*> field "metadata" metadata
        <*> field "content" content

tag :: Lens' (Tagged c) Key
tag = lens get set
  where
    get (Tagged k _ _) = k
    set (Tagged _ m c) k = Tagged k m c
{-# INLINE tag #-}

_Tagged :: Key -> Prism' (Tagged c) (Region c)
_Tagged key = prism (addTag key) $ \case
    Tagged k meta c
        | k == key -> Just $ Region meta c
    _              -> Nothing
{-# INLINE _Tagged #-}

-------------------------------------------------------------------------------
data Region content where
    Region :: Metadata -> content -> Region content
  deriving stock (Eq, Foldable, Functor, Show, Traversable)
  deriving (FromJSON, ToJSON) via (JSON (Region content))

instance HasContent (Region content) where
    type Content (Region content) = content
    content = lens get set
      where
        get (Region _ c)   = c
        set (Region m _) c = Region m c
    {-# INLINE content #-}

instance HasMetadata (Region c) where
    metadata = lens get set
      where
        get (Region m _)   = m
        set (Region _ c) m = Region m c
    {-# INLINE metadata #-}

instance (Typeable c, ToJSON c, FromJSON c) => Serde (Region c) where
    serde = Region
        <$> field "metadata" metadata
        <*> field "content" content

addTag :: Key -> Region c -> Tagged c
addTag key (Region m c) = Tagged key m c

-------------------------------------------------------------------------------
class HasContent node where
    type family Content node
    content :: Lens' node (Content node)

-------------------------------------------------------------------------------
class HasMetadata node where
    metadata :: Lens' node Metadata

properties :: HasMetadata node => Lens' node (Set Key)
properties = metadata . lens get set
  where
    get (Metadata xs _)    = xs
    set (Metadata _ ys) xs = Metadata xs ys
{-# INLINE properties #-}

property :: HasMetadata node => Key -> Lens' node Bool
property key = properties . lens get set
  where
    get         = Set.member key
    set m True  = Set.insert key m
    set m False = Set.delete key m
{-# INLINE property #-}

settings :: HasMetadata node => Lens' node (Map Key Text)
settings = metadata . lens get set
  where
    get (Metadata _ xs)    = xs
    set (Metadata ys _) xs = Metadata ys xs
{-# INLINE settings #-}

setting :: HasMetadata node => Key -> Lens' node (Maybe Text)
setting key = settings . lens get set
  where
    get            = Map.lookup key
    set m (Just v) = Map.insert key v m
    set m Nothing  = Map.delete key m
{-# INLINE setting #-}

-------------------------------------------------------------------------------
newtype NonEmpty f x where
    UncheckedNonEmpty :: f x -> NonEmpty f x
  deriving newtype (Eq, Foldable, Functor, Show, ToJSON)

instance (Foldable f, FromJSON (f x)) => FromJSON (NonEmpty f x) where
    parseJSON json = do
        inner <- parseJSON json
        maybe (fail "Empty sequence") pure $ nonEmpty inner

instance Traversable f => Traversable (NonEmpty f) where
    traverse f (UncheckedNonEmpty xs) =
        UncheckedNonEmpty <$> traverse f xs
    {-# INLINE traverse #-}

getNonEmpty :: NonEmpty f x -> f x
getNonEmpty (UncheckedNonEmpty xs) = xs
{-# INLINE getNonEmpty #-}

nonEmpty :: Foldable f => f x -> Maybe (NonEmpty f x)
nonEmpty xs
    | null xs   = Nothing
    | otherwise = Just (UncheckedNonEmpty xs)
{-# INLINE nonEmpty #-}
