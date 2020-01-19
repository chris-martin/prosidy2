{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Prosidy.Source 
    ( Source
    , sourcePath
    , sourceText
    , makeSource
    , nthLine
    , nthLineAndContext

    , SourceLocation
    , sourceLocationSource
    , sourceLocationOffset
    , sourceLocationLine
    , sourceLocationColumn
    , sourceLocation
    , prettySourceLocation

    -- , FromSource(..)
    -- , location
    -- , _FromSource

    , Line(..)
    , _Line
    , Column(..)
    , _Column
    , Offset(..)
    , _Offset
    ) where

import Prosidy.Internal.Optics

import Data.Foldable (foldl')
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (Text)
import Data.Word (Word64)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Data.Hashable (Hashable(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Binary (Binary)
import Control.DeepSeq (NFData)

-------------------------------------------------------------------------------
data Source where
    Source :: FilePath -> LineMap -> Text -> Source
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable, NFData, Binary)

instance Show Source where
    show (Source fp _ _) = "Source " <> show fp

sourcePath :: Lens' Source FilePath
sourcePath = lens get set 
  where
    get (Source p _ _)   = p
    set (Source _ l t) p = Source p l t
{-# INLINE sourcePath #-}

sourceText :: Lens' Source Text
sourceText = lens get set
  where
    get (Source _ _ t)   = t
    set (Source p l _) = Source p l
{-# INLINE sourceText #-}

makeSource :: FilePath -> Text -> Source
makeSource fp txt = Source fp (LineMap lm) txt
  where
    (_, _, lm) = foldl' lmFold (1, '\0', Map.singleton (Line 0) (Offset 0)) . zip [0..] $ Text.unpack txt
    lmFold (nth, prev, acc) (ix, ch)
        | ch ==  '\n' && prev == '\r' =
          (nth, ch, Map.insert (Line (pred nth)) (Offset (succ ix)) acc)
        | ch == '\n' || ch == '\r' =
          (succ nth, ch, Map.insert (Line nth) (Offset (succ ix)) acc)
        | otherwise =
          (nth, ch, acc)

nthLine :: Line -> Source -> Maybe Text
nthLine line (Source _ (LineMap lm) txt) =
    case (start, end) of
        (Nothing, Nothing) -> Nothing
        _ -> Just
           . maybe id Text.drop start 
           . maybe id (Text.dropEnd . (len -) . pred) end
           $ txt
  where
    len                = Text.length txt
    start              = offsetI <$> Map.lookup line lm
    end                = offsetI <$> Map.lookup (succ line) lm
    offsetI (Offset n) = fromIntegral n :: Int

nthLineAndContext :: Line -> Word64 -> Source -> [(Line, Text)]
nthLineAndContext (Line nth) n src = do
    let withLine x = (Line x,) <$> nthLine (Line x) src
    mapMaybe withLine [nth - n .. nth + n]

-------------------------------------------------------------------------------
newtype LineMap where
    LineMap :: Map Line Offset -> LineMap
  deriving stock (Eq, Generic)
  deriving anyclass (NFData, Binary)
  deriving newtype (Show)

instance Hashable LineMap where
    hashWithSalt salt (LineMap m) =
        Map.foldlWithKey' 
            (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
            salt
            m
-------------------------------------------------------------------------------
data SourceLocation where
    SourceLocation :: Source -> Offset -> ~Line -> ~Column -> SourceLocation
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Binary)

instance Eq SourceLocation where
    SourceLocation src o _ _ == SourceLocation src' o' _ _ =
        src == src' && o == o'

instance Hashable SourceLocation where
    hashWithSalt salt (SourceLocation src o _ _) =
        salt `hashWithSalt` src `hashWithSalt` o

sourceLocationSource :: Lens' SourceLocation Source
sourceLocationSource = lens get set
  where
    get (SourceLocation src _ _ _) = src
    set (SourceLocation _ os l c) src = SourceLocation src os l c
{-# INLINE sourceLocationSource #-}

sourceLocationLine :: Lens' SourceLocation Line
sourceLocationLine = lens get set
  where
    get (SourceLocation _ _ l _) = l
    set (SourceLocation src os _ c) l = SourceLocation src os l c
{-# INLINE sourceLocationLine #-}

sourceLocationColumn :: Lens' SourceLocation Column
sourceLocationColumn = lens get set
  where
    get (SourceLocation _ _ _ c) = c
    set (SourceLocation src os l _) = SourceLocation src os l
{-# INLINE sourceLocationColumn #-}

sourceLocationOffset :: Lens' SourceLocation Offset
sourceLocationOffset = lens get set
  where
    get (SourceLocation _ os _ _) = os
    set (SourceLocation src _ l c) os = SourceLocation src os l c
{-# INLINE sourceLocationOffset #-}

sourceLocation :: Source -> Offset -> Maybe SourceLocation
sourceLocation src@(Source _ (LineMap lm) _) offset@(Offset nthChar) = do
    (line, Offset lOffset) <- Map.foldrWithKey lineFold Nothing lm
    let column = Column $ nthChar - lOffset
    Just $ SourceLocation src offset line column
  where
    lineFold key val Nothing
      | val <= offset = Just (key, val)
      | otherwise     = Nothing
    lineFold _ _ x = x

prettySourceLocation :: SourceLocation -> String
prettySourceLocation (SourceLocation (Source path _ _) _ line col) =
    path <> ":" <> show line <> "," <> show col

-------------------------------------------------------------------------------
newtype Line = Line Word64
  deriving stock (Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Enum)
  deriving anyclass (Hashable, NFData, Binary)

instance Show Line where
    show (Line n) = show (succ n)

_Line :: Iso' Line Word64
_Line = iso (\(Line n) -> n) Line
{-# INLINE _Line #-}

newtype Column = Column Word64
  deriving stock (Eq, Ord ,Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving anyclass (Hashable, NFData, Binary)

instance Show Column where
    show (Column n) = show (succ n)

_Column :: Iso' Column Word64
_Column = iso (\(Column n) -> n) Column
{-# INLINE _Column #-}

newtype Offset = Offset Word64
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving anyclass (Hashable, NFData, Binary)

_Offset :: Iso' Offset Word64
_Offset = iso (\(Offset n) -> n) Offset
{-# INLINE _Offset #-}