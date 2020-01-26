{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Prosidy.Manual.TableOfContents
    ( TableOfContents(..)
    , TOCTree(..)
    , TOCFiles(..)
    , TOCEntry(..)
    , orderedEntries
    , tableOfContents
    , toList
    )
where

import           Prosidy
import           Prosidy.Manual.Slug

import qualified Control.Lens                  as L
import           Control.Lens.Operators
import           Data.Maybe                     ( fromMaybe, catMaybes )
import           Data.Binary                    ( Binary(..) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable(..) )
import           Control.DeepSeq                ( NFData )
import Control.Applicative ((<|>))
import Data.Text.Lens (packed)
import Data.Bifunctor (second)
import Control.Exception (Exception)
import Data.Text (Text)

data TableOfContents = TableOfContents
    { allPages    :: TOCFiles
    , thisPage    :: FileSlug
    , pageContent :: TOCEntry
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, NFData, Hashable)

newtype TOCFiles = TOCFiles (Map FileSlug Metadata)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, NFData)
  deriving newtype (Semigroup, Monoid)

instance Hashable TOCFiles where
    salt `hashWithSalt` TOCFiles tree =
        Map.foldlWithKey' (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v) 
            salt tree

newtype TOCTree = TOCTree [(Slug, TOCEntry)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, NFData, Hashable)
  deriving newtype (Semigroup, Monoid)

data TOCEntry = TOCEntry
  { entryTitle    :: Text
  , entryChildren :: TOCTree
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, NFData, Hashable)

type Assoc a b = [(a, b)]
type AssocItem a b = (a, b)

tableOfContents :: TOCFiles -> FilePath -> Document -> Either TOCError TableOfContents
tableOfContents tocFiles thisPath thisDoc = do
    (fakeSlug, tree) <- case handleRegion $ thisDoc ^. regionOf of
        Nothing -> Left $ NoTitle thisPath $ thisDoc ^. metadata
        Just x  -> Right x
    let slug = FileSlug (slugIndex fakeSlug) thisPath
    pure $ TableOfContents tocFiles slug tree

orderedEntries :: TableOfContents -> (Assoc FileSlug Metadata, AssocItem FileSlug TOCEntry, Assoc FileSlug Metadata)
orderedEntries (TableOfContents (TOCFiles others) thisSlug thisContent) =
    (before, (thisSlug, thisContent), after)
  where
    (before, after) = 
        second (dropWhile $ (== thisSlug) . fst)
      . span ((< thisSlug) . fst) 
      $ Map.toList others

handleRegion :: Region (Series Block) -> Maybe (Slug, TOCEntry)
handleRegion r = do
    case (r ^. setting "toc-title" <|> r ^. setting "title") of
        Nothing    -> Nothing
        Just title ->
          let thisPrio = priority r
              thisSlug = slug thisPrio $ fromMaybe title (r ^. setting "slug")
              children = r ^.. content . traverse . _BlockTag . _Tagged "section" . L.to handleRegion
              subtree  = TOCTree $ catMaybes children
          in Just (thisSlug, TOCEntry title subtree)

priority :: HasMetadata a => a -> Integer
priority = fromMaybe 0 . L.preview (setting "priority" . L._Just . L.re packed . L._Show)

toList :: TOCTree -> [(Slug, TOCEntry)]
toList (TOCTree t) = t

data TOCError =
    NoTitle FilePath Metadata
  deriving stock (Show, Eq)
  deriving anyclass Exception
