{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Prosidy.Compile.Internal.Info where

import qualified Prosidy                       as P

import           Data.HashMap.Strict            ( HashMap )
import           Type.Reflection                ( SomeTypeRep(..)
                                                , TypeRep
                                                , Typeable
                                                , typeRep
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.Hashable                  ( Hashable )
import           Data.HashSet                   ( HashSet )

import qualified Data.Text                     as Text

data InfoKey = InfoKey
    { inputType :: SomeTypeRep
    , rule      :: Text
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data Info =
    Product ProductInfo
  | Sum     SumInfo
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data PropertyInfo = PropertyInfo
    { name        :: P.Key
    , description :: Text
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data SettingInfo = SettingInfo
    { name        :: P.Key
    , description :: Text
    , usedAs      :: SomeTypeRep
    , required    :: Bool
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data ProductInfo = ProductInfo
    { key          :: InfoKey
    , description  :: Text
    , propertyInfo :: HashMap P.Key PropertyInfo
    , settingInfo  :: HashMap P.Key SettingInfo
    , descentKey   :: HashSet InfoKey
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

data SumInfo = SumInfo
    { key  :: InfoKey
    , info :: ProductInfo
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

infoKey :: forall a . Typeable a => Text -> InfoKey
infoKey = InfoKey (SomeTypeRep (typeRep @a))

displayInfoKey :: InfoKey -> String
displayInfoKey (InfoKey ty name) = show name <> " :: " <> show ty
