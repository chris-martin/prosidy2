{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module Prosidy.Compile.Internal.Eval where

import           Prosidy.Compile.Internal.Error ( Result
                                                , ResultT(..)
                                                , raiseError
                                                , mapErrors
                                                )
import           Prosidy.Types                  ( Key
                                                , HasMetadata
                                                , property
                                                , setting
                                                )

import           Control.Monad.Morph            ( MFunctor(..) )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Hashable                  ( Hashable )
import           GHC.Generics                   ( Generic )
import           Data.HashSet                   ( HashSet )
import           Data.Text                      ( Text )
import           Data.Generics.Product          ( field )
import           Data.Bifunctor                 ( first )

import qualified Control.Lens                  as L
import           Control.Lens.Operators

-- | A 'Functor' describing the interpretation of 'input' into 'output' under the
-- 'context' 'Monad'.
newtype Eval input context output = Eval
    { runEval :: input -> EvalState -> context (Result EvalError output, EvalState) }
  deriving (Functor, Applicative, Monad) via (EvalM input context)

instance MFunctor (Eval input) where
    hoist fn e = Eval $ \input state -> fn $ runEval e input state

instance MonadTrans (Eval input) where
    lift m = Eval $ \_ st -> fmap (\x -> (pure x, st)) m

-- | Keeps track of which properties and settings have been visited.
data EvalState = EvalState
    { _seenProperties :: HashSet Key
    , _seenSettings   :: HashSet Key
    }
  deriving (Generic)

wrapEvalError
    :: Functor context
    => Text
    -> Eval input context output
    -> Eval input context output
wrapEvalError name (Eval f) =
    Eval $ \i st -> first (mapErrors (WrappedEvalError name)) <$> f i st


seenProperties :: L.Lens' EvalState (HashSet Key)
seenProperties = field @"_seenProperties"

seenSettings :: L.Lens' EvalState (HashSet Key)
seenSettings = field @"_seenSettings"

-- | Various errors that can be raised during an evaluation.
data EvalError =
    ParseError Key String
  | MissingRequiredSetting Key
  | NoMatches [Text]
  | WrappedEvalError Text EvalError
  deriving (Eq, Generic, Show, Hashable)

evalProperty
    :: (HasMetadata input, Monad context) => Key -> Eval input context Bool
evalProperty key = evalM $ do
    seenProperties . L.contains key .= True
    L.view $ property key

evalOptionalSetting
    :: (HasMetadata input, Monad context)
    => (Text -> Either String a)
    -> Key
    -> Eval input context (Maybe a)
evalOptionalSetting parse key = evalM $ do
    value <- peekSetting parse key
    seenSettings . L.contains key .= True
    pure value

evalRequiredSetting
    :: (HasMetadata input, Monad context)
    => (Text -> Either String a)
    -> Key
    -> Eval input context a
evalRequiredSetting parse key = evalM $ do
    value <-
        peekSetting parse key
            >>= maybe (raiseError $ MissingRequiredSetting key) pure
    seenSettings . L.contains key .= True
    pure value

evaluate
    :: Monad context
    => Eval input context a
    -> input
    -> context (Result EvalError a)
evaluate = \e input -> fst <$> runEval e input (EvalState mempty mempty)

-------------------------------------------------------------------------------

type EvalM input context
    = ReaderT input (ResultT EvalError (StateT EvalState context))

evalM :: EvalM input context output -> Eval input context output
evalM e = Eval $ runStateT . runResultT . runReaderT e

peekSetting
    :: (HasMetadata input, Monad context)
    => (Text -> Either String a)
    -> Key
    -> EvalM input context (Maybe a)
peekSetting parse key = do
    maybeRaw <- L.view $ setting key
    either (raiseError . ParseError key) pure $ traverse parse maybeRaw
