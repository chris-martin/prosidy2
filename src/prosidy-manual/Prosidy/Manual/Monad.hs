{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Prosidy.Manual.Monad
    ( Manual
    , ManualError(..)
    , runManual
    , nestSection
    , recordTerm
    , headerTag
    )
where

import           Control.Applicative            ( liftA2 )
import           Control.Lens            hiding ( setting )
import           Control.Monad.Reader           ( ReaderT(..)
                                                , runReaderT
                                                , asks
                                                )
import           Control.Monad.State.Strict     ( StateT(..)
                                                , runStateT
                                                )
import           Data.HashSet                   ( HashSet )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Generics.Product          ( field )
import           Numeric.Natural                ( Natural )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Functor.Identity          ( Identity(..) )
import           Prosidy.Compile                ( Error )

import qualified Text.Blaze.Html5              as H
import qualified Data.Char                     as Char
import qualified Data.Text                     as Text
import qualified Data.HashSet                  as HashSet

newtype Manual a = Manual (ManualRead -> ManualState -> Either ManualError (a, ManualState))
  deriving (Functor, Applicative, Monad, MonadFix)
    via (ReaderT ManualRead (StateT ManualState (Either ManualError)))

instance Semigroup a => Semigroup (Manual a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Manual a) where
    mempty = pure mempty

data ManualRead = ManualRead
    { sectionDepth :: Natural
    }
  deriving (Eq, Generic, Show)

data ManualState = ManualState
    { definedTerms    :: HashSet Text
    , referencedTerms :: HashSet Text
    }
  deriving (Eq, Generic, Show)

data ManualError =
    CompileError Error
  | TooDeep
  | UndefinedTerms (HashSet Text)
  deriving (Eq, Show)

manualM
    :: ReaderT ManualRead (StateT ManualState (Either ManualError)) a
    -> Manual a
manualM m = Manual $ runStateT . runReaderT m

headerTag :: Manual (H.Html -> H.Html)
headerTag = manualM $ do
    depth <- asks sectionDepth
    case depth of
        0 -> pure H.h2
        1 -> pure H.h3
        2 -> pure H.h4
        3 -> pure H.h5
        4 -> pure H.h6
        _ -> ReaderT . const $ StateT . const $ Left TooDeep

nestSection :: Manual a -> Manual a
nestSection (Manual m) = Manual $ \r -> m $ over (field @"sectionDepth") succ r

runManual :: Manual a -> Either ManualError a
runManual (Manual m) = do
    (output, state) <- m (ManualRead 0) (ManualState mempty mempty)
    let undefinedTerms =
            referencedTerms state `HashSet.difference` definedTerms state
    if null undefinedTerms
        then Right output
        else Left $ UndefinedTerms undefinedTerms


-- | Record the use of a glossary term. Returns a sanitized identifier.
recordTerm
    :: Bool -- ^ Is this term the definition site that should be referenced by other occurances of the term?
    -> Text -- ^ This is the term text itself.
    -> Manual Text
recordTerm isDef term = manualM $ do
    selector . contains identifier .= True
    pure identifier
  where
    identifier = toIdentifier term
    selector | isDef     = field @"definedTerms"
             | otherwise = field @"referencedTerms"

-- | Convert text into an html-friendly identifier by replacing all
-- non-alphanumeric characters with hyphens and lower-casing the string. This
-- is less-permissive than HTML5's spec, but makes things a bit more readable.
toIdentifier :: Text -> Text
toIdentifier =
    Text.intercalate "-"
        . filter (not . Text.null)
        . Text.split (not . Char.isAlphaNum)
        . Text.toLower
