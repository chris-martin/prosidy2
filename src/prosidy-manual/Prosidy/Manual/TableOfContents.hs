{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Prosidy.Manual.TableOfContents where

import Prosidy
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
import Numeric.Natural (Natural)

data TocItem = TocItem
    { tocTitle    :: Text
    , tocSlug     :: Text
    , tocChildren :: [TocItem]
    }
  deriving Show

instance Binary TocItem where
    get = TocItem
        <$> fmap decodeUtf8 get
        <*> fmap decodeUtf8 get
        <*> get

    put (TocItem t s xs) = do
        put $ encodeUtf8 t
        put $ encodeUtf8 s
        put xs

calculateToc :: Natural -> Document -> TocItem
calculateToc depth = toTocItem depth . L.view _Document

foldToc :: Natural -> L.Fold (Region (Seq Block)) TocItem
foldToc 0 = const pure
foldToc depth = content . L.folded . allSections . L.to (toTocItem depth)
  where
    allSections :: L.Fold Block (Region (Seq Block))
    allSections = L.deepOf 
        (_BlockTag . spanning . content . L.folded) 
        (_BlockTag . spanning . _Tagged "section")

toTocItem :: Natural -> Region (Seq Block) -> TocItem
toTocItem depth r = TocItem navTitle slug $ r ^.. foldToc (pred depth)
  where
    realTitle = fromMaybe "UNTITLED" $ r ^. setting "title"
    navTitle  = fromMaybe realTitle  $ r ^. setting "nav-title"
    slug      = fromMaybe (toSlug realTitle) $ r ^. setting "slug"

toSlug :: Text -> Text
toSlug = Text.intercalate "-"
    . filter (not . Text.null)
    . Text.split (not . Char.isAlpha)
    . Text.toLower
