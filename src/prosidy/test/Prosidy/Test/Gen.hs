{-# LANGUAGE TypeFamilies #-}
module Prosidy.Test.Gen
    ( doc
    , block
    , inline
    , literal
    , blockTag
    , inlineTag
    , literalTag
    )
where

import           Hedgehog
import           Prosidy.Source (Spanned(..))
import           Prosidy.Types           hiding ( tag )

import           Data.Functor.Identity          ( Identity(..) )

import qualified Data.HashMap.Strict           as HashMap
import qualified Data.HashSet                  as HashSet
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

doc :: (MonadGen m, GenBase m ~ Identity) => m Document
doc = Document <$> meta <*> fmap spanned (Gen.seq (Range.linear 0 15) (Gen.small block))

block :: (MonadGen m, GenBase m ~ Identity) => m Block
block = Gen.recursive
    Gen.choice
    [ BlockParagraph . spanned <$> paragraph
    , BlockLiteral . spanned <$> tag (fmap spanned literal)
    ]
    [ BlockTag . spanned <$> (tag . fmap spanned $ Gen.seq (Range.linear 0 15) block) 
    ]

paragraph :: (MonadGen m, GenBase m ~ Identity) => m Paragraph
paragraph = do
    xs <- Gen.seq (Range.linear 1 15) inline
    maybe (fail "unreachable") (pure . Paragraph) (nonEmpty xs)

literal :: (MonadGen m, GenBase m ~ Identity) => m Literal
literal = Literal <$> Gen.text (Range.exponential 0 15) Gen.unicode

inline :: (MonadGen m, GenBase m ~ Identity) => m Inline
inline = Gen.recursive
    Gen.choice
    [ InlineText . spanned <$> Gen.text (Range.exponential 1 15) Gen.unicode
    , pure Break
    ]
    [InlineTag . spanned <$> tag (spanned <$> Gen.seq (Range.linear 0 15) inline) ]

key :: (MonadGen m, GenBase m ~ Identity) => m Key
key = do
    keyHead <- Gen.element
        "_abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    keyTail <- Gen.text (Range.linear 1 15)
        $ Gen.filter isValidKeyTail Gen.unicode
    pure . toKeyUnchecked $ Text.cons keyHead keyTail

tag :: (MonadGen m, GenBase m ~ Identity) => m c -> m (Tagged c)
tag content = Tagged <$> key <*> meta <*> Gen.small content

meta :: (MonadGen m, GenBase m ~ Identity) => m Metadata
meta = do
    props    <- Gen.set (Range.linear 0 15) key
    settings <- Gen.map
        (Range.linear 0 15)
        ((,) <$> key <*> Gen.text (Range.linear 0 15) Gen.unicode)
    pure $ Metadata props settings

blockTag :: (MonadGen m, GenBase m ~ Identity) => m BlockTag
blockTag = tag . fmap spanned $ Gen.seq (Range.linear 0 15) block

inlineTag :: (MonadGen m, GenBase m ~ Identity) => m InlineTag
inlineTag = tag . fmap spanned $ Gen.seq (Range.linear 0 15) inline

literalTag :: (MonadGen m, GenBase m ~ Identity) => m LiteralTag
literalTag = tag $ spanned <$> literal

spanned :: a -> Spanned a
spanned = Spanned Nothing