{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
module Prosidy.Manual.Monad where

import Prelude hiding (fail)

import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Monoid (Ap(..))
import Data.Functor.Identity (Identity(..))
import Control.Exception (Exception(..))
import Numeric.Natural (Natural)
import qualified Text.Blaze.Html5 as H
import Data.Text (Text)
import Data.HashSet (HashSet)
import Prosidy.Manual.Slug (Slug, slug, slugText)
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text

import Prosidy.Compile (CompileError)
import Prosidy.Manual.TableOfContents (TableOfContents)

newtype Manual a = Manual (ManualR -> ManualS -> (Either ManualError a, ManualS))
  deriving (Functor, Applicative, Monad)
    via (ReaderT ManualR (ExceptT ManualError (StateT ManualS Identity)))
  deriving (Semigroup, Monoid)
    via (Ap (ReaderT ManualR (ExceptT ManualError (StateT ManualS Identity))) a)

instance MonadFail Manual where
    fail = failWith . Fail

runManual :: Manual a -> FilePath -> TableOfContents -> Either ManualError a
runManual (Manual f) fp toc
  | HashSet.null undef = result
  | otherwise          = result *> Left (UndefinedReferences undef)
  where
    (result, st) = f (ManualR fp 0 toc) emptyState
    undef        = HashSet.difference (references st) (definitions st)

parameters :: Manual ManualR
parameters = Manual $ \params state -> (Right params, state)
{-# INLINE parameters #-}

parameter :: (ManualR -> a) -> Manual a
parameter f = Manual $ \params state -> (Right $ f params, state)
{-# INLINE parameter #-}

getState :: Manual ManualS
getState = Manual $ \_ state -> (Right state, state)

setState :: ManualS -> Manual ()
setState state = Manual $ \_ _ -> (Right (), state)

failWith :: ManualError -> Manual a
failWith e = Manual $ \_ state -> (Left e, state)

data ManualR = ManualR
  { currentPath     :: FilePath
  , currentDepth    :: Natural
  , tableOfContents :: TableOfContents
  }

data ManualS = ManualS
  { definitions :: HashSet Slug
  , references  :: HashSet Slug
  }

emptyState :: ManualS
emptyState = ManualS mempty mempty

nesting :: Manual a -> Manual a
nesting (Manual r) = Manual $ \mr ->
    r mr { currentDepth = succ $ currentDepth mr }

headerTag :: Manual (H.Html -> H.Html)
headerTag = headerTagAt <$> parameter currentDepth
  where
    headerTagAt 0 = H.h2
    headerTagAt 1 = H.h3
    headerTagAt 2 = H.h4
    headerTagAt 3 = H.h5
    headerTagAt 4 = H.h6
    headerTagAt _ = \x -> H.h6 $ x <> " (Exceeded 5 levels of nesting!)"

define :: Text -> Manual Slug
define lemma = do
    state <- getState
    let defs      = definitions state
        lemmaSlug = slug lemma
    if HashSet.member lemmaSlug defs
        then failWith $ DuplicateDefinitions lemma
        else setState state { definitions = HashSet.insert lemmaSlug defs } *> pure lemmaSlug

reference :: Text -> Manual Slug
reference lemma = do
    let lemmaSlug = slug lemma
    state <- getState
    setState state { references = HashSet.insert lemmaSlug (references state) }
    pure lemmaSlug

data ManualError =
    Fail String
  | CompileError CompileError
  | DuplicateDefinitions Text
  | UndefinedReferences (HashSet Slug)
  deriving (Show, Eq)

instance Exception ManualError where
    displayException (Fail msg) = 
        "MonadFail: " <> msg

    displayException (CompileError e) = 
        displayException e

    displayException (DuplicateDefinitions d) =
        "The term " <> show d <> " was defined multiple times."

    displayException (UndefinedReferences rs) =
        "The following references have no corresponding definition: " <>
        Text.unpack (Text.intercalate ", " $ slugText <$> HashSet.toList rs)