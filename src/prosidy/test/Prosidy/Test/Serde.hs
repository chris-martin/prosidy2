module Prosidy.Test.Serde
    ( test
    )
where

import           Test.Tasty
import           Test.Tasty.Hedgehog

import qualified Data.Aeson                    as Aeson
import qualified Prosidy.Test.Gen              as Gen
import qualified Hedgehog                      as H

import qualified Data.ByteString.Lazy.Char8    as LC8

test :: TestTree
test = testGroup "(de)serialization" [involution]

involution :: TestTree
involution = testGroup
    "involution"
    [ testProperty "LiteralTag" $ prop_involution Gen.literalTag
    , testProperty "InlineTag" $ prop_involution Gen.inlineTag
    , testProperty "BlockTag" $ prop_involution Gen.blockTag
    , testProperty "Inline" $ prop_involution Gen.inline
    , testProperty "Block" $ prop_involution Gen.block
    , testProperty "Doc" $ prop_involution Gen.doc
    ]

prop_involution
    :: (Show a, Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => H.Gen a -> H.Property
prop_involution gen = H.property $ do
    item <- H.forAll gen
    let encoded = Aeson.encode item
        decoded = Aeson.eitherDecode' encoded
    H.footnote $ LC8.unpack encoded
    decoded H.=== Right item
