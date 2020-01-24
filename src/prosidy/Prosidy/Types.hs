{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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
    ( -- * the top level document
      Document(..)

      -- * Block contexts
    , Block(..)
    , _BlockLiteral
    , _BlockTag
    , _BlockParagraph

      -- * Inline contexts
    , Inline(..)
    , _InlineTag
    , _InlineFragment
    , _Break

      -- * Tags
    , Tagged(..)
    , BlockTag
    , InlineTag
    , LiteralTag
    , tag
    , _Tagged

      -- * Untagged regions
    , Region(..)
    , HasContent(..)
    , addTag
    , regionOf

      -- * Literals
    , Literal(..)

      -- * Paragraphs
    , Paragraph(..)

      -- * Plain text fragments
    , Fragment(..)

      -- * Keys
    , Key
    , pattern Key
    , keyName
    , keyQ
    , toKey
    , toKeyUnchecked
    , isValidKeyHead
    , isValidKeyTail

      -- * Series
    , Series(..)
    , _Series

      -- * Metadata
    , Metadata(..)
    , HasMetadata(..)
    , properties
    , property
    , settings
    , setting

      -- * Source tagging
    , FromSource(..)

      -- * Non-empty foldables
    , NonEmpty
    , nonEmpty
    , getNonEmpty
    ) where

import Prosidy.Internal.Optics
import Prosidy.Internal.JSON
import Prosidy.Source

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Aeson (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..))
import Data.Aeson.Types (FromJSONKeyFunction(..))
import Data.String (IsString(..))
import Type.Reflection (Typeable)
import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Foldable (asum, foldl', toList)

import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as Q
import qualified Data.Sequence as Seq
import qualified Data.Binary as Binary

-------------------------------------------------------------------------------
data Block where
    BlockTag       :: BlockTag   -> Block
    BlockParagraph :: Paragraph  -> Block
    BlockLiteral   :: LiteralTag -> Block
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)
  deriving (FromJSON, ToJSON) via (JSON Block)

instance FromSource Block where
    location = affine get set
      where
        get (BlockTag       x) = preview location x
        get (BlockParagraph x) = preview location x
        get (BlockLiteral   x) = preview location x
        set (BlockTag       x) src = BlockTag       $ assign location src x
        set (BlockParagraph x) src = BlockParagraph $ assign location src x
        set (BlockLiteral   x) src = BlockLiteral   $ assign location src x

instance Serde Block where
    serde = finalizeSerde $ asum
        [ choice "tag" (Just "literal") _BlockLiteral
        , choice "paragraph" Nothing _BlockParagraph
        , choice "tag" (Just "block") _BlockTag
        , noMatch "Expected a LiteralTag, BlockTag, or Paragraph"
        ]

type BlockTag = Tagged (Series Block)

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
    Document :: Metadata -> Series Block -> Document
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON) via (JSON Document)

instance FromSource Document where
    location = nullAffine

instance HasContent Document where
    type Content Document = Series Block
    content = lens get set
      where
        get (Document _ c) = c
        set (Document m _) = Document m
    {-# INLINE content #-}

instance HasMetadata Document where
    metadata = lens get set
      where
        get (Document m _)   = m
        set (Document _ c) m = Document m c
    {-# INLINE metadata #-}

instance Serde Document where
    serde = finalizeSerde $ Document
        <$> field "metadata" metadata
        <*> field "content"  content

-------------------------------------------------------------------------------
data Inline where
    Break          :: Inline
    InlineTag      :: InlineTag -> Inline
    InlineFragment :: Fragment  -> Inline
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (FromJSON, ToJSON) via (JSON Inline)

instance FromSource Inline where
    location = affine get set
      where
        get Break = Nothing
        get (InlineTag x) = preview location x
        get (InlineFragment x) = preview location x

        set Break _ = Break
        set (InlineTag x) src = InlineTag $ assign location src x
        set (InlineFragment x) src = InlineFragment $ assign location src x

instance Serde Inline where
    serde = finalizeSerde $ asum
        [ choice "break" Nothing _Break
        , choice "tag" (Just "inline") _InlineTag
        , choice "text" Nothing _InlineFragment
        , noMatch "Expected an Break, InlineTag, or Inline Text"
        ]

type InlineTag = Tagged (Series Inline)

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

_InlineFragment :: Prism' Inline Fragment
_InlineFragment = prism InlineFragment $ \case
    InlineFragment text -> Just text
    _                   -> Nothing
{-# INLINE _InlineFragment #-}

-------------------------------------------------------------------------------
newtype Key where
    UncheckedKey :: Text -> Key
  deriving newtype (Eq, Hashable, Ord, Show, ToJSON, ToJSONKey)
  deriving stock (Generic)
  deriving anyclass (NFData, Binary)

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
    unsupported kind _ = fail $ kind <> " are not supported by the 'key' quasiquoter."

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
data Literal where
    Literal :: Text -> Maybe SourceLocation -> Literal
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON) via (JSON Literal)

instance FromSource Literal where
    location = affine get set
      where
        get (Literal _ src) = src
        set (Literal txt _) = Literal txt . Just
    {-# INLINE location #-}

instance HasContent Literal where
    type Content Literal = Text
    content = lens get set
      where
        get (Literal t _) = t
        set (Literal _ src) t = Literal t src
    {-# INLINE content #-}

instance Serde Literal where
    serde = wrapper (view content) (flip Literal Nothing)

-------------------------------------------------------------------------------
data Metadata where
    Metadata :: Set Key -> Map Key Text -> Metadata
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Binary)
  deriving (FromJSON, ToJSON) via (JSON Metadata)

instance HasMetadata Metadata where
    metadata = id
    {-# INLINE metadata #-}

instance Hashable Metadata where
    hashWithSalt salt (Metadata ks ss) =
        Map.foldlWithKey'
          (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
          (foldl' hashWithSalt salt ks)
          ss

instance Monoid Metadata where
    mempty = Metadata mempty mempty

instance Semigroup Metadata where
    Metadata x1 x2 <> Metadata y1 y2 =
        Metadata (x1 <> y1) (x2 <> y2)

instance Serde Metadata where
    serde = finalizeSerde $ Metadata
        <$> field "properties" properties
        <*> field "settings"   settings

-------------------------------------------------------------------------------
data Paragraph where
    Paragraph :: NonEmpty Series Inline -> Maybe SourceLocation -> Paragraph
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON) via JSON Paragraph

instance FromSource Paragraph where
    location = affine get set
      where
        get (Paragraph _ src) = src
        set (Paragraph ct _) = Paragraph ct . Just
    {-# INLINE location #-}

instance HasContent Paragraph where
    type Content Paragraph = NonEmpty Series Inline
    content = lens get set
      where
        get (Paragraph ct _) = ct
        set (Paragraph _ src) ct = Paragraph ct src
    {-# INLINE content #-}

instance Serde Paragraph where
    serde = wrapper (view content) (flip Paragraph Nothing)

-------------------------------------------------------------------------------
data Tagged content where
    Tagged :: Key -> Metadata -> content -> Maybe SourceLocation -> Tagged content
  deriving stock (Eq, Foldable, Functor, Show, Traversable, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (FromJSON, ToJSON) via (JSON (Tagged content))

instance FromSource (Tagged content) where
    location = affine get set
      where
        get (Tagged _ _ _ src) = src
        set (Tagged key md x _) = Tagged key md x . Just
    {-# INLINE location #-}

instance HasContent (Tagged content) where
    type Content (Tagged content) = content
    content = lens get set
      where
        get (Tagged _ _ c _) = c
        set (Tagged k m _ src) c = Tagged k m c src
    {-# INLINE content #-}

instance HasMetadata (Tagged c) where
    metadata = lens get set
      where
        get (Tagged _ m _ _)   = m
        set (Tagged k _ c src) m = Tagged k m c src
    {-# INLINE metadata #-}

instance (Typeable c, ToJSON c, FromJSON c) => Serde (Tagged c) where
    serde = finalizeSerde $ Tagged
        <$> field "name" tag
        <*> field "metadata" metadata
        <*> field "content" content
        <*> pure Nothing

tag :: Lens' (Tagged c) Key
tag = lens get set
  where
    get (Tagged k _ _ _) = k
    set (Tagged _ m c src) k = Tagged k m c src
{-# INLINE tag #-}

_Tagged :: Key -> Prism' (Tagged c) (Region c)
_Tagged key = prism (addTag key) $ \case
    Tagged k meta c src
        | k == key -> Just $ Region meta c src
    _              -> Nothing
{-# INLINE _Tagged #-}

-------------------------------------------------------------------------------
data Fragment where
    Fragment :: Text -> Maybe SourceLocation -> Fragment
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (ToJSON, FromJSON) via JSON Fragment

instance FromSource Fragment where
    location = affine get set
      where
        get (Fragment _ src) = src
        set (Fragment txt _) = Fragment txt . Just
    {-# INLINE location #-}

instance HasContent Fragment where
    type Content Fragment = Text
    content = lens get set
      where
        get (Fragment txt _) = txt
        set (Fragment _ src) txt = Fragment txt src
    {-# INLINE content #-}

instance Serde Fragment where
    serde = wrapper (view content) (flip Fragment Nothing)

-------------------------------------------------------------------------------
data Region content where
    Region :: Metadata -> content -> Maybe SourceLocation -> Region content
  deriving stock (Eq, Foldable, Functor, Show, Traversable, Generic)
  deriving anyclass (Hashable, NFData, Binary)
  deriving (FromJSON, ToJSON) via (JSON (Region content))

instance FromSource (Region content) where
    location = affine get set
      where
        get (Region _ _ src) = src
        set (Region md x _) = Region md x . Just
    {-# INLINE location #-}

instance HasContent (Region content) where
    type Content (Region content) = content
    content = lens get set
      where
        get (Region _ c _) = c
        set (Region m _ src) c = Region m c src
    {-# INLINE content #-}

instance HasMetadata (Region c) where
    metadata = lens get set
      where
        get (Region m _ _)   = m
        set (Region _ c src) m = Region m c src
    {-# INLINE metadata #-}

instance (Typeable c, ToJSON c, FromJSON c) => Serde (Region c) where
    serde = finalizeSerde $ Region 
        <$> field "metadata" metadata
        <*> field "content" content
        <*> pure Nothing

addTag :: Key -> Region c -> Tagged c
addTag key (Region m c src) = Tagged key m c src

regionOf :: (HasMetadata i, HasContent i, FromSource i) => Lens' i (Region (Content i))
regionOf = lens get set
  where
    get = Region <$> view metadata <*> view content <*> preview location
    set x (Region md ct loc) = 
        maybe id (assign location) loc
        . assign metadata md
        . assign content ct
        $ x
{-# INLINE regionOf #-}

-------------------------------------------------------------------------------
class HasContent node where
    type family Content node
    content :: Lens' node (Content node)

class FromSource node where
    location :: Affine' node SourceLocation

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
    get (Metadata _ xs) = xs
    set (Metadata ys _) = Metadata ys
{-# INLINE settings #-}

setting :: HasMetadata node => Key -> Lens' node (Maybe Text)
setting key = settings . lens get set
  where
    get            = Map.lookup key
    set m (Just v) = Map.insert key v m
    set m Nothing  = Map.delete key m
{-# INLINE setting #-}

-------------------------------------------------------------------------------
newtype Series a = Series (Seq a)
  deriving newtype (Eq, Foldable, Functor, Applicative, Show, ToJSON, FromJSON, NFData, Semigroup, Monoid)
  deriving stock (Generic)

instance Binary a => Binary (Series a) where
    get = Series . Seq.fromList <$> Binary.get
    put (Series xs) = Binary.put $ toList xs

instance Hashable a => Hashable (Series a) where
    hashWithSalt salt (Series xs) = foldl' hashWithSalt salt xs

instance Traversable Series where
    traverse f (Series xs) = Series <$> traverse f xs

_Series :: Iso' (Series a) (Seq a)
_Series = iso (\(Series xs) -> xs) Series
{-# INLINE _Series #-}

-------------------------------------------------------------------------------
newtype NonEmpty f x where
    UncheckedNonEmpty :: f x -> NonEmpty f x
  deriving newtype (Eq, Foldable, Functor, Show, ToJSON, Generic, Hashable, NFData, Binary)

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
