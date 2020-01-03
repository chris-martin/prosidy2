{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prosidy.Internal.JSON
    ( Serde(..)
    , JSON(..)
    , field
    , choice
    , noMatch
    )
where

import           Prosidy.Internal.Optics

import           Control.Applicative            ( Alternative(..) )
import           Data.Aeson.Types               ( Parser
                                                , Object
                                                , parserCatchError
                                                , prependFailure
                                                )
import           Data.Monoid                    ( Endo(Endo, appEndo) )
import           Data.Profunctor                ( Profunctor(..) )
import           Data.Text                      ( Text )
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                )
import           Type.Reflection                ( Typeable
                                                , typeRep
                                                )
import           Control.Monad                  ( unless )

import qualified Data.Aeson                    as Aeson

newtype JSON s = JSON s

instance Serde s => FromJSON (JSON s) where
    parseJSON =
        Aeson.withObject (show $ typeRep @s) $ fmap JSON . deserialize serde

instance Serde s => ToJSON (JSON s) where
    toJSON (JSON x) = Aeson.object $ appEndo (serialize serde x) []

    toEncoding (JSON x) =
        Aeson.pairs . mconcat $ appEndo (serialize serde x) []

class Typeable s => Serde s where
    serde :: SerdeA s s

choice
    :: (FromJSON a, ToJSON a) => Text -> Maybe Text -> Prism' s a -> SerdeA s s
choice typeName subtypeName sel = SerdeA
    { serialize   = let chosen item =
                            single ("value" .= item)
                                <> single ("type" .= typeName)
                                <> foldMap (single . ("subtype" .=)) subtypeName
                    in  foldMap chosen . preview sel
    , deserialize = \obj -> do
                        typeName' <- obj .: "type"
                        unless (typeName == typeName')
                            $  fail
                            $  "expected type="
                            <> show typeName
                            <> ", got type="
                            <> show typeName'
                        subtypeName' <- obj .:? "subtype"
                        unless (subtypeName == subtypeName')
                            $  fail
                            $  "expected subtype="
                            <> show subtypeName
                            <> ", got subtype="
                            <> show subtypeName'
                        review sel <$> obj .: "value"
    }

noMatch :: String -> SerdeA s a
noMatch msg = SerdeA mempty (const $ fail msg)

field :: (FromJSON a, ToJSON a) => Text -> Lens' s a -> SerdeA s a
field name sel = SerdeA { serialize   = \item -> single $ name .= view sel item
                        , deserialize = \obj -> obj .: name
                        }

data SerdeA t a = SerdeA
    { serialize   :: forall kv. Aeson.KeyValue kv => t -> Endo [kv]
    , deserialize :: Object -> Parser a
    }

instance Typeable s => Alternative (SerdeA s) where
    empty = SerdeA
        mempty
        (const . fail $ "No remaining parsers for " <> show (typeRep @s))

    SerdeA s1 d1 <|> SerdeA s2 d2 =
        SerdeA (s1 <> s2)
            $ \o -> parserCatchError (d1 o)
                  $ \_ msg -> prependFailure (msg ++ "; ") (d2 o)

instance Applicative (SerdeA s) where
    pure x = SerdeA mempty (const $ pure x)

    SerdeA s1 d1 <*> SerdeA s2 d2 = SerdeA (s1 <> s2) (\o -> d1 o <*> d2 o)

instance Functor (SerdeA s) where
    fmap = dimap id

instance Profunctor SerdeA where
    dimap lhs rhs (SerdeA ser deser) = SerdeA (ser . lhs) (fmap rhs . deser)

single :: a -> Endo [a]
single = Endo . (:)
