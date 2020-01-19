{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}
module Prosidy.Manual.TableOfContents 
    ( TableOfContents(..)
    , Entry(..)
    , insertEntry
    , documentEntry
    , foldlEntries
    , foldrEntries
    ) where

import Prosidy
import Prosidy.Manual.Slug

import Data.Sequence (Seq)
import Data.Text (Text)
import Control.Applicative ((<|>))
import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Binary (Binary(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector (Vector)
import Numeric.Natural (Natural)
import qualified Data.Vector as Vector
import Data.Map.Strict (Map)
import qualified Data.Text.Lens as TL
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Control.DeepSeq (NFData)

newtype TableOfContents = TableOfContents (Map FileSlug Entry)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, NFData)
  deriving newtype (Semigroup, Monoid)

instance Hashable TableOfContents where
    hashWithSalt salt (TableOfContents xs) =
        Map.foldlWithKey' (\x k v -> x `hashWithSalt` k `hashWithSalt` v) salt xs

data Entry = Entry
    { entryTitle    :: Text
    , entrySlug     :: Slug
    , entryChildren :: Vector Entry
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Binary Entry where
    get = Entry
        <$> get
        <*> get
        <*> fmap Vector.fromList get

    put (Entry t s xs) = do
        put t
        put s
        put $ Vector.toList xs

instance Hashable Entry where
    hashWithSalt salt (Entry title slug children) =
        Vector.foldl' hashWithSalt (salt `hashWithSalt` title `hashWithSalt` slug) children

insertEntry :: FilePath -> Entry -> TableOfContents -> TableOfContents
insertEntry fpath dEntry (TableOfContents toc) = TableOfContents toc'
    where
      dSlug  = FileSlug (slugIndex $ entrySlug dEntry) fpath
      toc'   = L.set (L.at dSlug) (Just dEntry) toc

foldrEntries :: (Entry -> a -> a) -> a -> TableOfContents -> a
foldrEntries f ini (TableOfContents toc) = Map.foldr f ini toc

foldlEntries :: (a -> Entry -> a) -> a -> TableOfContents -> a
foldlEntries f ini (TableOfContents toc) = Map.foldl' f ini toc

documentEntry :: Document -> Entry
documentEntry = regionToEntry . L.view regionOf

regionToEntry :: Region (Series Block) -> Entry
regionToEntry region = Entry rTitle rSlug rChildren
  where
    rTitle    = fromMaybe "<UNTITLED>" $
        region ^? L.failing 
            (setting "toc-title" . L._Just)
            (setting "title"     . L._Just)
    rIndex    = fromMaybe 0 $ region ^? setting "priority" . L._Just . L.re TL.packed . L._Show
    rSlug     = (slug $ fromMaybe rTitle (region ^. setting "slug")) { slugIndex = rIndex }
    rChildren = Vector.fromList $ region ^.. entryFold

entryFold :: L.Fold (Region (Series Block)) Entry
entryFold = content . L.folded . allSections . L.to regionToEntry
  where
    allSections = L.deepOf 
        (_BlockTag . content . L.folded)
        (_BlockTag . _Tagged "section")
