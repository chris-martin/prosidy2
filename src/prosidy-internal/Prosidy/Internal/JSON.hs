{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Prosidy.Internal.JSON
    ( Serde(..)
    , JSON(..)
    , SerdeFinal
    , finalizeSerde
    , wrapper
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
                                                , Encoding
                                                , Value
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                )
import           Type.Reflection                ( Typeable
                                                , typeRep
                                                )
import           Control.Monad                  ( unless )

import qualified Data.Aeson                    as Aeson

newtype JSON s = JSON { unJSON :: s }

instance Serde s => FromJSON (JSON s) where
    parseJSON = fmap JSON . serdeParseJSON serde
    {-# INLINE parseJSON #-}

instance Serde s => ToJSON (JSON s) where
    toJSON     = serdeToJSON serde . unJSON
    toEncoding = serdeToEncoding serde . unJSON
    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

class Serde s where
    serde :: SerdeFinal s

choice
    :: (FromJSON a, ToJSON a)
    => Text
    -> Maybe Text
    -> Prism' s a
    -> SerdeBuild s s
choice typeName subtypeName sel = SerdeBuild
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

noMatch :: String -> SerdeBuild s a
noMatch msg = SerdeBuild mempty (const $ fail msg)

field :: (FromJSON a, ToJSON a) => Text -> Lens' s a -> SerdeBuild s a
field name sel = SerdeBuild
    { serialize   = \item -> single $ name .= view sel item
    , deserialize = \obj -> obj .: name
    }

data SerdeFinal t = SerdeFinal
    { serdeToEncoding    :: t -> Encoding
    , serdeToJSON        :: t -> Value
    , serdeParseJSON     :: Value -> Parser t
    }

finalizeSerde :: forall t . Typeable t => SerdeBuild t t -> SerdeFinal t
finalizeSerde (SerdeBuild s d) = SerdeFinal
    { serdeToEncoding = Aeson.pairs . mconcat . flip appEndo [] . s
    , serdeToJSON     = Aeson.object . flip appEndo [] . s
    , serdeParseJSON  = Aeson.withObject (show $ typeRep @t) d
    }

wrapper :: (FromJSON u, ToJSON u) => (t -> u) -> (u -> t) -> SerdeFinal t
wrapper to fro = SerdeFinal { serdeToEncoding = toEncoding . to
                            , serdeToJSON     = toJSON . to
                            , serdeParseJSON  = fmap fro . parseJSON
                            }

data SerdeBuild t a = SerdeBuild
    { serialize   :: forall kv. Aeson.KeyValue kv => t -> Endo [kv]
    , deserialize :: Object -> Parser a
    }

instance Typeable s => Alternative (SerdeBuild s) where
    empty = SerdeBuild
        mempty
        (const . fail $ "No remaining parsers for " <> show (typeRep @s))

    SerdeBuild s1 d1 <|> SerdeBuild s2 d2 =
        SerdeBuild (s1 <> s2)
            $ \o -> parserCatchError (d1 o)
                  $ \_ msg -> prependFailure (msg ++ "; ") (d2 o)

instance Applicative (SerdeBuild s) where
    pure x = SerdeBuild mempty (const $ pure x)

    SerdeBuild s1 d1 <*> SerdeBuild s2 d2 =
        SerdeBuild (s1 <> s2) (\o -> d1 o <*> d2 o)

instance Functor (SerdeBuild s) where
    fmap = dimap id

instance Profunctor SerdeBuild where
    dimap lhs rhs (SerdeBuild ser deser) =
        SerdeBuild (ser . lhs) (fmap rhs . deser)

single :: a -> Endo [a]
single = Endo . (:)
