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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Prosidy.Compile
    ( Compile
    , ProductT
    , SumT
    , Desc
    , Item
    , Choice
    , Error(..)
    , tags
    , tagRule
    , blockRule
    , inlineRule
    , documentRule
    , paragraphRule
    , rule
    , access
    , staticOnly
    , compileM
    , compile
    , self
    , prop
    , req
    , opt
    , child
    , children
    , embed
    , disallow
    ) where

import qualified Prosidy.Compile.Internal.Eval as Eval
import qualified Prosidy.Compile.Internal.Spec as Spec
import qualified Prosidy.Compile.Internal.Util as Util
import qualified Control.Lens                  as L
import Control.Lens.Operators

import Data.Sequence (Seq)
import           Prosidy.Compile.Internal.Spec  ( ItemKey(..) )
import           Prosidy.Compile.Internal.Error ( Errors
                                                , Result(..)
                                                , foldResult
                                                , resultError
                                                , liftResult
                                                , eachError
                                                , mapErrors
                                                , raiseError
                                                )
import           Prosidy.Types                  ( Key
                                                , HasMetadata
                                                , Tagged
                                                , Region
                                                , Paragraph
                                                , Literal
                                                , LiteralTag
                                                , BlockTag
                                                , InlineTag
                                                , Block
                                                , Inline
                                                , _BlockTag
                                                , _BlockLiteral
                                                , _InlineTag
                                                , _BlockParagraph
                                                , _InlineText
                                                , _Literal
                                                , _Break
                                                , _Tagged
                                                , _Paragraph
                                                , Document
                                                , HasContent(..)
                                                , pattern Key) 
import Prosidy.Source (Spanned(..), Span, spanning, spanOf) 
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
import           Data.Profunctor                ( Profunctor(..) )
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

type ProductT input context output = Compile (Item input context output)
type SumT input context output = Compile (Choice input context output)

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

instance MFunctor (Item item) where
    hoist f (Item desc eval) = Item desc $ f . eval

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
    { description  :: Text
    , propertyInfo :: HashMap Key Spec.PropertyInfo
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
    => ProductT input context output
    -> input
    -> context (Either Error output)
compileM (Compile c) input = runExceptT $ do
    item   <- liftEither specResult
    result <- lift $ evalItem item input
    foldResult (throwError . EvalError) pure result
  where
    specResult :: Either Error (Item input context output)
    (specResult, _) = c mempty

compile :: ProductT input Identity output -> input -> Either Error output
compile c = runIdentity . compileM c

-------------------------------------------------------------------------------
tagRule :: forall input output context. (Typeable input, Typeable output, Monad context) 
    => Key
    -> Text
    -> Desc (Region input) context output
    -> SumT (Spanned (Tagged input)) context output
tagRule key@(Key name) description desc = liftCompileM $ do
    (info, doEval) <- prepare name description desc
    pure . Choice info $ \item ->
      let 
        runEval    = wrapErrors name (item ^? spanOf) . Eval.evaluate doEval
        region'    = L.preview (spanning . _Tagged key) item
      in 
        runEval <$> region'

blockRule :: (Typeable output, Monad context) 
          => Text
          -> Item (Spanned Paragraph)  context output
          -> Item (Spanned LiteralTag) context output
          -> Item (Spanned BlockTag)   context output
          -> ProductT Block context output
blockRule name onPg onLit onBlock = choose name
    [ oneChoice _BlockTag onBlock
    , oneChoice _BlockLiteral onLit
    , oneChoice _BlockParagraph onPg
    ] 

inlineRule :: (Typeable output, Monad context)
          => Text
          -> context output
          -> (Text -> context output)
          -> Item (Spanned InlineTag)  context output
          -> ProductT Inline context output
inlineRule name onBreak onText onTag = choose name
    [ oneChoice _InlineTag onTag
    , oneChoice _InlineText $ Item
        { itemMetadata = ItemInfo
            { ruleKey = Spec.itemKey @Text (name <> " text")
            , ruleInfo = Product $ ProductInfo
                { description  = "plain text"
                , propertyInfo = mempty
                , settingInfo  = mempty
                , descentKey   = Nothing
                }
            }
        , evalItem = \item -> fmap Ok . onText $ L.view spanning item
        }
    , oneChoice _Break $ Item
        { itemMetadata = ItemInfo
            { ruleKey = Spec.itemKey @() (name <> " break")
            , ruleInfo = Product $ ProductInfo
                { description  = "break"
                , propertyInfo = mempty
                , settingInfo  = mempty
                , descentKey   = Nothing
                }
            }
        , evalItem = const $ Ok <$> onBreak
        }
    ]

paragraphRule :: forall context output. (Typeable output, Monad context, Monoid (context (Result Eval.EvalError output)))
    => Item Inline context output
    -> (output -> context output)
    -> ProductT (Spanned Paragraph) context output
paragraphRule item wrapContent = liftCompileM $ do
    (info, doEval) <- prepare "paragraph" "description" $
        descend item $ spanning . _Paragraph . L.folded
    pure . Item info $ \input -> do
        let span = input ^? spanOf
            wrap = wrapErrors "paragraph" span
        result <- wrap $ Eval.evaluate doEval input
        traverse wrapContent result

documentRule :: forall output context. (Typeable output, Monad context) 
    => Desc Document context output
    -> ProductT Document context output
documentRule desc = liftCompileM $ do
    (info, doEval) <- prepare "document" "top-level document" desc
    pure . Item info $ Eval.evaluate doEval

rule :: forall input output context. (Typeable input, Typeable output, Monad context) 
     => Text 
     -> Text
     -> Desc input context output 
     -> ProductT input context output
rule name description desc  = liftCompileM $ do
    (info, doEval) <- prepare name description desc 
    pure . Item info $ Eval.evaluate doEval

prepare :: forall input output context. (Typeable input, Typeable output, Monad context) 
     => Text 
     -> Text
     -> Desc input context output 
     -> CompileM (ItemInfo, Eval.Eval input context output)
prepare name desc (Desc spec) = do
    (doEval, specState) <- case Spec.specify spec of
        Ok   ok -> pure ok
        Fail e  -> throwError . SpecError . eachError (Spec.Wrapped name) $ e
    let Spec.SpecState props settings inner = specState
        ikey  = Spec.itemKey @(Spanned (Tagged input)) name
        pinfo = ProductInfo desc props settings inner
        info  = ItemInfo ikey (Product pinfo)
    Util.setOnceFail (L.at ikey) (RuleConflict ikey) info
    pure (info, doEval)

tags :: forall input context output
     . (Typeable input, Typeable output, Monad context)
    => Text
    -> [Choice (Spanned (Tagged input)) context output]
    -> ProductT (Spanned (Tagged input)) context output
tags name choices = liftCompileM $ do
    let ikey  = Spec.itemKey @(Spanned (Tagged input)) name
        sinfo = SumInfo $ fmap choiceMetadata choices
        iinfo = ItemInfo ikey (Sum sinfo)
    Util.setOnceFail (L.at ikey) (RuleConflict ikey) iinfo
    pure . Item iinfo $ \input ->
        case asum $ fmap (flip evalChoice input) choices of
            Just result -> result
            Nothing -> 
              let
                failures = fmap (ruleName . ruleKey . choiceMetadata) choices
                theError = Eval.Wrapped name (input $> Eval.NoMatches failures)
              in
                pure $ resultError theError

-------------------------------------------------------------------------------
children :: (Monoid (context (Result Eval.EvalError output)), Content input ~ Seq child, HasContent input, Monad context)
    => Item child context output
    -> Desc input context output
children onChild = descend onChild (content . L.folded)

child :: (HasContent input, Monad context)
    => Item (Content input) context output
    -> Desc input context output
child onChild = descend onChild content

disallow :: forall input context output. (Typeable input, Monad context) => Text -> Item input context output
disallow message = Item itemInfo . const . pure . resultError $ Eval.CustomError message
  where
    itemKey  = Spec.itemKey @input "disallowed"
    itemInfo = ItemInfo itemKey (Product pInfo)
    pInfo    = ProductInfo message mempty mempty Nothing
    
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

self :: Monad context => Desc input context input
self = descM $ pure ask

-------------------------------------------------------------------------------
type CompileM = ExceptT Error (State Registry)

liftCompileM :: CompileM a -> Compile a
liftCompileM (ExceptT (StateT f)) = Compile $ runIdentity . f

descM :: Spec.SpecM (Eval.EvalM i m a) -> Desc i m a
descM = Desc . Spec.specM . fmap Eval.evalM

choose :: forall input context output. (Typeable input, Typeable output, Monad context)
    => Text
    -> [Choice input context output]
    -> ProductT input context output
choose name choices = liftCompileM $ do
    let key   = Spec.itemKey @input name
        sinfo = SumInfo $ fmap choiceMetadata choices
        info  = ItemInfo key (Sum sinfo)
    Util.setOnceFail (L.at key) (RuleConflict key) info
    pure . Item info $ \input ->
        case asum $ fmap (flip evalChoice input) choices of
            Just result -> result
            Nothing ->
                pure . resultError . Eval.Wrapped name . Spanned Nothing $ Eval.NoMatches
                    (fmap (ruleName . ruleKey . choiceMetadata) choices)

oneChoice :: Util.Affine' input choice
    -> Item choice context output
    -> Choice input context output
oneChoice sel item = Choice
    { choiceMetadata = itemMetadata item
    , evalChoice     = fmap (evalItem item) . L.preview sel
    }

wrapErrors :: Functor c => Text -> Maybe Span -> c (Result Eval.EvalError o) -> c (Result Eval.EvalError o)
wrapErrors name span = fmap (mapErrors $ Eval.Wrapped name . Spanned span)
