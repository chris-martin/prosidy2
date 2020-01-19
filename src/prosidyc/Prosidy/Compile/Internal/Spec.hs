{-# LANGUAGE AllowAmbiguousTypes #-}
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
module Prosidy.Compile.Internal.Spec where

import           Prosidy.Types                  ( Key )
import           Prosidy.Compile.Internal.Info  ( InfoKey(..)
                                                , PropertyInfo(..)
                                                , SettingInfo(..)
                                                )
import           Prosidy.Compile.Internal.Util  ( setOnce
                                                , typeOf
                                                )
import           Prosidy.Compile.Internal.Error ( Result
                                                , ResultT(..)
                                                )

import           Data.Text                      ( Text )
import           Data.HashMap.Strict            ( HashMap )
import           Type.Reflection                ( Typeable
                                                , SomeTypeRep(..)
                                                , typeRep
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Control.Monad.State            ( StateT(..)
                                                , State
                                                , runState
                                                )
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Generics.Product          ( field )

import qualified Control.Lens                  as L

newtype Spec output = Spec
    { runSpec :: SpecState -> (Result SpecError output, SpecState) }
  deriving (Functor, Applicative, Monad)
    via SpecM

data SpecState = SpecState
    { _propertyMap :: HashMap Key PropertyInfo
    , _settingMap  :: HashMap Key SettingInfo
    , _descendWith :: Maybe InfoKey
    }
  deriving stock (Eq, Generic, Show)

data SpecError =
    SpecFail String
  | PropertyConflict Key PropertyInfo
  | SettingConflict Key SettingInfo
  | RuleConflict InfoKey
  | SubruleConflict InfoKey
  | WrappedSpecError InfoKey SpecError
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

specify :: Spec a -> Result SpecError (a, SpecState)
specify s = fmap (, state) result
    where (result, state) = runSpec s $ SpecState mempty mempty Nothing

specifyProperty :: Key -> Text -> Spec ()
specifyProperty key description = specM $ do
    let info = PropertyInfo key description
    setOnce (propertyMap . L.at key) (PropertyConflict key) info

specifySetting
    :: forall output . (Typeable output) => Bool -> Key -> Text -> Spec ()
specifySetting required key description = specM $ do
    let info = SettingInfo key description (typeOf @output) required
    setOnce (settingMap . L.at key) (SettingConflict key) info

-------------------------------------------------------------------------------
type SpecM = ResultT SpecError (State SpecState)

specM :: SpecM a -> Spec a
specM = Spec . runState . runResultT

propertyMap :: L.Lens' SpecState (HashMap Key PropertyInfo)
propertyMap = field @"_propertyMap"

settingMap :: L.Lens' SpecState (HashMap Key SettingInfo)
settingMap = field @"_settingMap"

descendWith :: L.Lens' SpecState (Maybe InfoKey)
descendWith = field @"_descendWith"
