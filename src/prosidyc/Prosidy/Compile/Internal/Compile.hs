{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Prosidy.Compile.Internal.Compile where

import qualified Prosidy as P

import qualified Prosidy.Compile.Internal.Error as Error
import qualified Prosidy.Compile.Internal.Eval  as Eval
import qualified Prosidy.Compile.Internal.Spec  as Spec
import qualified Prosidy.Compile.Internal.Util  as Util
import qualified Prosidy.Compile.Internal.Info  as Info

import Control.Monad.Trans (lift)
import Data.Foldable (asum)
import qualified Control.Lens                   as L
import           Control.Lens.Operators
import Data.Generics.Product (field, position)
import Data.Generics.Sum.Constructors (_Ctor)
import Data.Functor (($>))
import           Control.Monad.Morph            ( MFunctor(..) )
import           Control.Monad.Reader           ( ask )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , liftEither
                                                , throwError
                                                )
import           Control.Monad.State.Strict     ( State
                                                , StateT(..)
                                                )
import           Data.HashMap.Strict            ( HashMap )
import           Data.Functor.Identity          ( Identity(..) )
import Control.Monad.Fix (MonadFix)
import GHC.Generics (Generic)
import Data.Functor.Compose (Compose(..))
import Data.Profunctor (Profunctor(..))
import Data.Monoid (Ap(..))
import Type.Reflection (Typeable)
import Data.Text (Text)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.HashSet as HashSet
import Control.Exception (Exception(..))
import Control.Monad.Morph (MFunctor(..))

newtype Compile a = Compile
    (Registry -> (Either CompileError a, Registry))
  deriving (Functor, Applicative, Monad, MonadFix)
    via CompileM

data CompileError =
    SpecError (Error.Errors Spec.SpecError)
  | EvalError (Error.Errors Eval.EvalError)
  deriving stock (Eq, Show, Generic)

instance Exception CompileError where
    displayException (SpecError s) = show s
    displayException (EvalError e) = displayException e

-------------------------------------------------------------------------------
newtype Desc input context output = Desc
    (Spec.Spec (Eval.Eval input context output))
  deriving (Functor, Applicative)
    via (Compose Spec.Spec (Eval.Eval input context))

instance MFunctor (Desc input) where
    hoist f (Desc spec) = Desc (fmap (hoist f) spec)

-------------------------------------------------------------------------------
type Product input context output = Compile (ProductRule input context output)

data ProductRule input context output = ProductRule
    { metadata :: Info.ProductInfo
    , evaluate :: input -> Ap context (Error.Result Eval.EvalError output)
    }
  deriving stock (Generic)

instance Functor context => Functor (ProductRule input context) where
    fmap f (ProductRule m e) = ProductRule m $ fmap (fmap f) . e

instance MFunctor (ProductRule input) where
    hoist f (ProductRule md eval) = ProductRule md $ \input ->
        Ap . f . getAp $ eval input

type Sum input context output = Compile (SumRule input context output)

data SumRule input context output = SumRule
    { metadata :: Info.SumInfo
    , evaluate :: input -> Maybe (Ap context (Error.Result Eval.EvalError output))
    }
  deriving stock (Generic)

instance MFunctor (SumRule input) where
    hoist f (SumRule md eval) = SumRule md $ \input ->
        Ap . f . getAp <$> eval input

instance Functor context => Functor (SumRule input context) where
    fmap f (SumRule m e) = SumRule m $ fmap (fmap (fmap f)) . e

annotateProduct :: (P.FromSource input, Functor context) => ProductRule input context output -> ProductRule input context output
annotateProduct ~(ProductRule meta eval) = ProductRule meta $ \input ->
    Error.mapErrors (Eval.WrappedEvalError (meta ^. field @"key") (input ^? P.location)) <$> eval input

annotateSum :: (P.FromSource input, Functor context) => SumRule input context output -> SumRule input context output
annotateSum ~(SumRule meta eval) = SumRule meta $ \input ->
    fmap (Error.mapErrors (Eval.WrappedEvalError (meta ^. field @"key") (input ^? P.location))) <$> eval input

-------------------------------------------------------------------------------
newtype Registry = Registry (HashMap Info.InfoKey Info.Info)
  deriving (Semigroup, Monoid)
    via (HashMap Info.InfoKey Info.Info)

instance L.Ixed Registry where

instance L.At Registry where
    at k = _Registry . L.at k

type instance L.Index Registry = Info.InfoKey
type instance L.IxValue Registry = Info.Info

_Registry :: L.Iso' Registry (HashMap Info.InfoKey Info.Info)
_Registry = L.coerced

-------------------------------------------------------------------------------
compileRegistry :: Compile a -> Either CompileError Registry
compileRegistry (Compile c) = result $> registry
  where
    (result, registry) = c $ Registry mempty

compile :: Product input Identity output -> input -> Either CompileError output
compile c = runIdentity . compileM c

compileM :: Monad context => Product input context output -> input -> context (Either CompileError output)
compileM (Compile c) input = runExceptT $ do
    ProductRule _ eval <- liftEither . fst . c $ Registry mempty
    result <- lift . getAp $ eval input
    case result of
        Error.Ok ok  -> pure ok
        Error.Fail e -> throwError $ EvalError e

-------------------------------------------------------------------------------
productRule :: 
       forall input output context. Monad context
    => Info.InfoKey
    -> Text
    -> Desc input context output
    -> Product input context output
productRule thisKey description (Desc spec) = liftCompileM $ do 
    (runEval, specState) <- case Spec.specify spec of
      Error.Ok ok -> pure ok
      Error.Fail e -> throwError $ SpecError e
    let ~(Spec.SpecState ps ss innerInfo) = specState
        thisProductInfo = Info.ProductInfo thisKey description ps ss $
                          foldMap HashSet.singleton innerInfo
        thisInfo = Info.Product thisProductInfo
    Util.setOnceFail 
        (L.at thisKey) 
        (SpecError . Error.singleton $ Spec.RuleConflict thisKey)
        thisInfo
    pure $ ProductRule thisProductInfo (Ap . Eval.evaluate runEval)

sumRule :: 
     forall choice input output context. Monad context
  => Util.Affine' input choice
  -> Info.InfoKey
  -> ProductRule choice context output
  -> Sum input context output
sumRule sel thisKey ~(ProductRule meta eval) = liftCompileM $ do
    let thisSumInfo = Info.SumInfo thisKey meta
        thisInfo    = Info.Sum thisSumInfo
    Util.setOnceFail 
        (L.at thisKey)
        (SpecError . Error.singleton $ Spec.RuleConflict thisKey)
        thisInfo
    pure . SumRule thisSumInfo . L.preview $ sel . L.to eval 

chooseOne :: 
      forall input output context. Monad context
    => Info.InfoKey
    -> Text
    -> [SumRule input context output]
    -> Product input context output
chooseOne thisKey description choices = liftCompileM $ do
    let thisMetadata    = fmap (L.view $ field @"metadata") choices
        subRuleKeys     = HashSet.fromList $ choices ^.. traverse . field @"metadata" . field @"key"
        thisProductInfo = Info.ProductInfo thisKey description mempty mempty subRuleKeys
        thisInfo        = Info.Product thisProductInfo
    Util.setOnceFail 
        (L.at thisKey)
        (SpecError . Error.singleton $ Spec.RuleConflict thisKey)
        thisInfo
    pure $ ProductRule thisProductInfo $ \input ->
        case asum $ fmap (flip (L.view (field @"evaluate")) input) choices of
          Just result -> result
          Nothing -> pure . Error.resultError $ Eval.NoMatches subRuleKeys

disallow :: forall input output context. (Typeable input, Applicative context) => Text -> ProductRule input context output
disallow message = ProductRule
    { metadata = Info.ProductInfo 
        { key          = Info.infoKey @input "Disallowed"
        , description  = "Disallowed: " <> message
        , propertyInfo = mempty
        , settingInfo  = mempty
        , descentKey  = mempty
        }
    , evaluate = const . pure . Error.resultError $ Eval.CustomError message
    }

-------------------------------------------------------------------------------
prop
    :: (P.HasMetadata input, Monad context)
    => P.Key
    -> Text
    -> Desc input context Bool
prop name description = Desc $ do
    Spec.specifyProperty name description
    pure $ Eval.evalProperty name

reqParse
    :: forall input context output
     . (P.HasMetadata input, Monad context, Typeable output)
    => (Text -> Either String output)
    -> P.Key
    -> Text
    -> Desc input context output
reqParse parse name description = Desc $ do
    Spec.specifySetting @output True name description
    pure $ Eval.evalRequiredSetting parse name

optParse
    :: forall input context output
     . (P.HasMetadata input, Monad context, Typeable output)
    => (Text -> Either String output)
    -> P.Key
    -> Text
    -> Desc input context (Maybe output)
optParse parse name description = Desc $ do
    Spec.specifySetting @output False name description
    pure $ Eval.evalOptionalSetting parse name

embed :: Monad context => context a -> Desc input context a
embed = Desc . pure . lift

access :: Monad context => (input -> context a) -> Desc input context a
access f = Desc . pure . Eval.evalM $ ask >>= lift . lift . lift . f

descendWith :: (P.FromSource input, Monad context) => Descent context output input child -> ProductRule child context output -> Desc input context output
descendWith sel ~(ProductRule md eval) = Desc $ do
    pure . Eval.Eval $ \input state ->
      let
        result = getAp $ input ^. sel . L.to eval
        mapErr = Error.mapErrors (Eval.WrappedEvalError (md ^. field @"key") (input ^? P.location))
      in
        fmap ((, state) . mapErr) result
    
self :: Applicative context => Desc input context input
self = Desc . pure . Eval.Eval $ \i st -> pure (Error.Ok i, st)

-------------------------------------------------------------------------------
type CompileM = ExceptT CompileError (State Registry)

type Descent context output input child =
    L.Getting (Ap context (Error.Result Eval.EvalError output)) input child 

liftCompileM :: CompileM a -> Compile a
liftCompileM (ExceptT (StateT f)) = Compile $ runIdentity . f