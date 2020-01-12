{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
module Prosidy.Source where

import Prosidy.Internal.Optics

import qualified Data.Text as Text

import Data.Foldable (foldl')
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import Data.Word (Word)
import Data.Sequence (Seq)

-------------------------------------------------------------------------------
data Source where
    Source :: FilePath -> Text -> Source
  deriving stock (Eq, Show)

sourcePath :: Lens' Source FilePath
sourcePath = lens get set 
  where
    get (Source p _)   = p
    set (Source _ t) p = Source p t
{-# INLINE sourcePath #-}

sourceText :: Lens' Source Text
sourceText = lens get set
  where
    get (Source _ t)   = t
    set (Source p _) = Source p
{-# INLINE sourceText #-}

-------------------------------------------------------------------------------
data Span where
    Span :: Source -> Offset -> Offset -> Span
  deriving stock (Eq, Show)

spanSource :: Lens' Span Source
spanSource = lens get set
  where
    get (Span s _ _) = s
    set (Span _ f t) s = Span s f t
{-# INLINE spanSource #-}

spanBegin :: Lens' Span Offset
spanBegin = lens get set
  where
    get (Span _ f _) = f
    set (Span s _ t) f = Span s f t
{-# INLINE spanBegin #-}

spanEnd :: Lens' Span Offset
spanEnd = lens get set
  where
    get (Span _ _ t) = t
    set (Span s f _) = Span s f
{-# INLINE spanEnd #-}

-------------------------------------------------------------------------------
data SpanDetail where 
    SpanDetail :: Source -> Position -> Position -> Text -> SpanDetail
  deriving (Show, Eq)

spanDetail :: Span -> SpanDetail
spanDetail (Span source@(Source _ srcText) (Offset from) (Offset to)) =
    SpanDetail source startPosition endPosition spanned
  where
    (before, srcText') = Text.splitAt (fromIntegral from) srcText
    (spanned, _)      = Text.splitAt (fromIntegral to) srcText'
    startPosition      = advancePosition (Position (Line 0) (Column 0)) before
    endPosition        = advancePosition startPosition spanned

advancePosition :: Position -> Text -> Position
advancePosition p = fst . foldl' (uncurry go) (p, '\0') . Text.unpack
  where
    go pos '\r' '\n' = (pos, '\n')
    go (Position (Line line) (Column col)) _ ch
      | ch == '\n' || ch == '\r' = (Position (Line $ succ line) (Column 0), ch)
      | otherwise = (Position (Line line) (Column $ succ col), ch)

-------------------------------------------------------------------------------
data Spanned a where
    Spanned :: Maybe Span -> a -> Spanned a
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

instance FromJSON a => FromJSON (Spanned a) where
    parseJSON = fmap (Spanned Nothing) . parseJSON

instance ToJSON a => ToJSON (Spanned a) where
    toEncoding (Spanned _ x) = toEncoding x
    toJSON     (Spanned _ x) = toJSON x

spanning :: Lens (Spanned a) (Spanned b) a b
spanning = lens get set
  where 
    get (Spanned _ x)  = x
    set (Spanned ms _) = Spanned ms
{-# INLINE spanning #-}

spanOfMaybe :: Lens' (Spanned a) (Maybe Span)
spanOfMaybe = lens get set 
  where 
    get (Spanned ms _)  = ms
    set (Spanned _ a)   = flip Spanned a
{-# INLINE spanOfMaybe #-}

spanOf :: Affine' (Spanned a) Span
spanOf = affine get set
  where 
    get (Spanned ms _)  = ms
    set (Spanned _ a)   = flip Spanned a . Just
{-# INLINE spanOf #-}

type SpannedSeq a = Spanned (Seq (Spanned a))

-------------------------------------------------------------------------------
data Position where
    Position :: Line -> Column -> Position
  deriving (Show, Eq)

newtype Line = Line Word
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

_Line :: Iso' Line Word
_Line = iso (\(Line n) -> n) Line
{-# INLINE _Line #-}

newtype Column = Column Word
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

_Column :: Iso' Column Word
_Column = iso (\(Column n) -> n) Column
{-# INLINE _Column #-}

newtype Offset = Offset Word
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

_Offset :: Iso' Offset Word
_Offset = iso (\(Offset n) -> n) Offset
{-# INLINE _Offset #-}