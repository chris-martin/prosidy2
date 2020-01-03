{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Prosidy.Compile
    ( Compile
    , RuleT
    , Rule
    , Desc
    , Item
    , Choice
    , Error(..)
    , access
    , staticOnly
    , compileM
    , compile
    , rule
    , choose
    , self
    , prop
    , req
    , opt
    , descend
    , embed
    , contextualize
    , (@?)
    )
where

import qualified Prosidy.Compile.Internal.Eval as Eval
import qualified Prosidy.Compile.Internal.Spec as Spec
import qualified Prosidy.Compile.Internal.Util as Util
import qualified Control.Lens                  as L

import           Prosidy.Compile.Internal.Spec  ( ItemKey(..) )
import           Prosidy.Compile.Internal.Error ( Errors
                                                , Result(..)
                                                , foldResult
                                                , resultError
                                                , liftResult
                                                )

import           Prosidy.Types                  ( Key
                                                , HasMetadata
                                                )

import           Data.Functor                   ( ($>) )
import           Data.Foldable                  ( asum )
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
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Hashable                  ( Hashable )
import           GHC.Generics                   ( Generic )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Text                      ( Text )
import           Control.Monad.Fix              ( MonadFix )
import           Data.Functor.Compose           ( Compose(..) )
import           Type.Reflection                ( Typeable )
import           Data.HashMap.Strict            ( HashMap )


newtype Compile a = Compile
    (Registry -> (Either Error a, Registry))
  deriving (Functor, Applicative, Monad, MonadFix)
    via (CompileM)

data Error =
    SpecError (Errors Spec.SpecError)
  | EvalError (Errors Eval.EvalError)
  | RuleConflict ItemKey
  deriving stock (Eq, Show, Generic)

type Rule input output = RuleT input Identity output

type RuleT input context output = Compile (Item input context output)

newtype Desc input context output = Desc
    (Spec.Spec (Eval.Eval input context output))
  deriving (Functor, Applicative)
    via (Compose Spec.Spec (Eval.Eval input context))

instance MFunctor (Desc input) where
    hoist f (Desc spec) = Desc (fmap (hoist f) spec)

data Item input context output = Item
    { itemMetadata :: ItemInfo
    , evalItem     :: input -> context (Result Eval.EvalError output)
    }

data Choice input context output = Choice
    { choiceMetadata :: ItemInfo
    , evalChoice     :: input -> Maybe (context (Result Eval.EvalError output))
    }

data ItemInfo = ItemInfo
    { ruleKey  :: ItemKey
    , ruleInfo :: AlgebraicInfo
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data AlgebraicInfo =
    Product ProductInfo
  | Sum     SumInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data ProductInfo = ProductInfo
    { propertyInfo :: HashMap Key Spec.PropertyInfo
    , settingInfo  :: HashMap Key Spec.SettingInfo
    , descentKey   :: Maybe ItemKey
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

newtype SumInfo = SumInfo [ItemInfo]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

newtype Registry = Registry (HashMap ItemKey ItemInfo)
  deriving (Semigroup, Monoid)
    via (HashMap ItemKey ItemInfo)

instance L.Ixed Registry where

instance L.At Registry where
    at k = _Registry . L.at k

type instance L.Index Registry = ItemKey
type instance L.IxValue Registry = ItemInfo

_Registry :: L.Iso' Registry (HashMap ItemKey ItemInfo)
_Registry = L.coerced

-------------------------------------------------------------------------------
staticOnly :: Compile a -> Either Error Registry
staticOnly (Compile c) = result $> registry
    where (result, registry) = c $ Registry mempty

compileM
    :: forall input context output
     . Monad context
    => RuleT input context output
    -> input
    -> context (Either Error output)
compileM (Compile c) input = runExceptT $ do
    item   <- liftEither specResult
    result <- lift $ evalItem item input
    foldResult (throwError . EvalError) pure result
  where
    specResult :: Either Error (Item input context output)
    (specResult, _) = c mempty

compile :: Rule input output -> input -> Either Error output
compile c = runIdentity . compileM c

-------------------------------------------------------------------------------
rule
    :: forall input context output
     . (Typeable input, Typeable output, Monad context)
    => Text
    -> Desc input context output
    -> RuleT input context output
rule name (Desc spec) = liftCompileM $ do
    (eval, Spec.SpecState props settings into) <-
        foldResult (throwError . SpecError) pure $ Spec.specify spec
    let key   = ItemKey name (Util.typeOf @input) (Util.typeOf @output)
        pinfo = ProductInfo props settings into
        info  = ItemInfo key (Product pinfo)
    Util.setOnceFail (L.at key) (RuleConflict key) info
    pure $ Item info (Eval.evaluate eval)

choose
    :: forall input context output
     . (Typeable input, Typeable output, Monad context)
    => Text
    -> [Choice input context output]
    -> RuleT input context output
choose name choices = liftCompileM $ do
    let key   = ItemKey name (Util.typeOf @input) (Util.typeOf @output)
        sinfo = SumInfo $ fmap choiceMetadata choices
        info  = ItemInfo key (Sum sinfo)
    Util.setOnceFail (L.at key) (RuleConflict key) info
    pure . Item info $ \input ->
        case asum $ fmap (flip evalChoice input) choices of
            Just result -> result
            Nothing     -> pure $ resultError Eval.NoMatches

infix 3 @?
(@?)
    :: Util.Affine' input choice
    -> Item choice context output
    -> Choice input context output
a @? item = Choice { choiceMetadata = itemMetadata item
                   , evalChoice     = fmap (evalItem item) . L.preview a
                   }

-------------------------------------------------------------------------------
descend
    :: Monad context
    => Item child context output
    -> L.Getting (context (Result Eval.EvalError output)) input child
    -> Desc input context output
descend (Item info eval) via = descM $ do
    Util.setOnce Spec.descendWith Spec.SubruleConflict $ ruleKey info
    pure $ do
        resultM <- L.view $ via . L.to eval
        result  <- lift . lift . lift $ resultM
        liftResult result

prop
    :: (HasMetadata input, Monad context)
    => Key
    -> Text
    -> Desc input context Bool
prop name description = Desc $ do
    Spec.specifyProperty name description
    pure $ Eval.evalProperty name

req
    :: forall input context output
     . (HasMetadata input, Monad context, Typeable output)
    => (Text -> Either String output)
    -> Key
    -> Text
    -> Desc input context output
req parse name description = Desc $ do
    Spec.specifySetting @output True name description
    pure $ Eval.evalRequiredSetting parse name

opt
    :: forall input context output
     . (HasMetadata input, Monad context, Typeable output)
    => (Text -> Either String output)
    -> Key
    -> Text
    -> Desc input context (Maybe output)
opt parse name description = Desc $ do
    Spec.specifySetting @output False name description
    pure $ Eval.evalOptionalSetting parse name

embed :: Monad context => context a -> Desc input context a
embed = Desc . pure . lift

access :: Monad context => (input -> context a) -> Desc input context a
access f = Desc . pure . Eval.evalM $ ask >>= lift . lift . lift . f

contextualize
    :: Monad context
    => (forall x . context x -> context' x)
    -> Item input context a
    -> Item input context' a
contextualize f (Item desc eval) = Item desc $ f . eval

self :: Monad context => Desc input context input
self = descM $ pure ask

-------------------------------------------------------------------------------
type CompileM = ExceptT Error (State Registry)

liftCompileM :: CompileM a -> Compile a
liftCompileM (ExceptT (StateT f)) = Compile $ runIdentity . f

descM :: Spec.SpecM (Eval.EvalM i m a) -> Desc i m a
descM = Desc . Spec.specM . fmap Eval.evalM
