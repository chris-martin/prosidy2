{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
module Prosidy.Compile.Internal.Eval where

import           Prosidy.Compile.Internal.Info  ( InfoKey
                                                , displayInfoKey
                                                )
import           Prosidy.Compile.Internal.Error ( Result(Fail)
                                                , ResultT(..)
                                                , raiseError
                                                , singleton
                                                )
import           Prosidy.Types                  ( Key
                                                , HasMetadata
                                                , property
                                                , setting
                                                , properties
                                                , settings
                                                )
import           Prosidy.Source                 ( SourceLocation
                                                , prettySourceLocation
                                                , nthLineAndContext
                                                , sourceLocationSource
                                                , sourceLocationLine
                                                )
import           Control.Exception              ( Exception(..) )

import           Control.Monad.Morph            ( MFunctor(..) )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Control.Monad.State.Strict     ( StateT(..) )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Hashable                  ( Hashable )
import           GHC.Generics                   ( Generic )
import           Data.HashSet                   ( HashSet )
import           Data.Text                      ( Text )
import           Data.Generics.Product          ( field )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map

import qualified Data.HashSet                  as HashSet
import qualified Data.Text                     as Text
import qualified Control.Lens                  as L
import           Control.Lens.Operators
import           Control.Monad                  ( unless )

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

seenProperties :: L.Lens' EvalState (HashSet Key)
seenProperties = field @"_seenProperties"

seenSettings :: L.Lens' EvalState (HashSet Key)
seenSettings = field @"_seenSettings"

exhaustive :: (Applicative context, HasMetadata input) => Eval input context ()
exhaustive = Eval $ \input state@(EvalState props sets) ->
    let missingProps = foldMap
            (\key -> if HashSet.member key props
                then mempty
                else singleton $ UnknownProperty key
            )
            (input ^. properties)
        missingSettings = foldMap
            (\key -> if HashSet.member key sets
                then mempty
                else singleton $ UnknownSetting key
            )
            (input ^. settings . L.to Map.keysSet)
        allErrors = missingProps <> missingSettings
    in  pure
            ( unless (null missingProps && null missingSettings)
                     (Fail allErrors)
            , state
            )

-- | Various errors that can be raised during an evaluation.
data EvalError =
    ParseError Key String
  | MissingRequiredSetting Key
  | NoMatches (HashSet InfoKey)
  | WrappedEvalError InfoKey (Maybe SourceLocation) EvalError
  | UnknownSetting Key
  | UnknownProperty Key
  | CustomError Text
  deriving (Eq, Generic, Show, Hashable)

instance Exception EvalError where
    displayException (ParseError key message) =
        "Error while parsing " <> show key <> ": " <> message

    displayException (MissingRequiredSetting key) =
        "Missing required setting: " <> show key

    displayException (NoMatches possibilities) = intercalate
        "\n"
        ( "Failed to match any possible rules. The following rules were tried:"
        : (("  - " <>) . displayInfoKey <$> HashSet.toList possibilities)
        )

    displayException (CustomError message) = "Error: " <> Text.unpack message

    displayException (WrappedEvalError key mbLoc inner@WrappedEvalError{}) =
        intercalate
            "\n"
            [ "While processing "
            <> displayInfoKey key
            <> foldMap ((" at " <>) . prettySourceLocation) mbLoc
            , displayException inner
            ]

    displayException (WrappedEvalError key Nothing inner) =
        "While proccessing the contents of "
            <> displayInfoKey key
            <> "\n\n"
            <> displayException inner

    displayException (WrappedEvalError key (Just loc) inner) =
        "While proccessing the contents of "
            <> displayInfoKey key
            <> " at " <> prettySourceLocation loc
            <> "\n\n"
            <> (let showLine nth line =
                          Text.justifyRight 4 ' ' (Text.pack $ show nth)
                              <> " | "
                              <> line
                in
                    Text.unpack
                    . Text.intercalate "\n"
                    . fmap (uncurry showLine)
                    $ nthLineAndContext (loc ^. sourceLocationLine)
                                        3
                                        (loc ^. sourceLocationSource)
               )
            <> "\n\n"
            <> displayException inner

    displayException (UnknownSetting  k) = "Unknown setting " <> show k

    displayException (UnknownProperty k) = "Unknown property " <> show k

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
evaluate e input = fst <$> runEval e input (EvalState mempty mempty)

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
