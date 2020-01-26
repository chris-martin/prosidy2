{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, GeneralisedNewtypeDeriving, OverloadedStrings #-}
module Prosidy.Manual.Slug
    ( Slug
    , slug
    , slugText
    , slugIndex
    , FileSlug(..)
    )
where

import Prosidy

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Char                     as Char
import           Data.Binary                    ( Binary(..) )
import           Control.DeepSeq                ( NFData(..) )
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Text.Blaze.Html5               ( ToValue(..) )

data FileSlug = FileSlug
    { fileSlugIndex :: Integer
    , fileSlugPath  :: FilePath
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance Ord FileSlug where
    FileSlug ix0 p0 `compare` FileSlug ix1 p1 = 
        case ix0 `compare` ix1 of
            GT -> LT
            LT -> GT
            EQ -> p0 `compare` p1

instance Binary FileSlug where
    get = FileSlug <$> get <*> get
    put s = do
        put $ fileSlugIndex s
        put $ fileSlugPath s

data Slug = Slug
    { slugIndex :: Integer
    , slugText  :: Text
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance Binary Slug where
    get = Slug <$> get <*> get
    put s = do
        put $ slugIndex s
        put $ slugText s

instance Ord Slug where
    Slug ix0 t0 `compare` Slug ix1 t1 = case ix0 `compare` ix1 of
        EQ -> t0 `compare` t1
        GT -> LT
        LT -> GT

instance ToValue Slug where
    toValue = toValue . slugText

slug :: Integer -> Text -> Slug
slug i =
    Slug i
        . Text.intercalate "-"
        . filter (not . Text.null)
        . Text.split (not . Char.isAlpha)
        . Text.toLower