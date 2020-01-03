{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Prosidy.Test.Optics
    ( test
    )
where

import           Test.Tasty

test :: TestTree
test = testGroup "Optics" mempty

-- import           Control.Lens            hiding ( rewrite )
-- import           Prosidy.Optics
-- import           Test.Tasty
-- import           Test.Tasty.Hedgehog
-- import           Hedgehog                       ( (===) )
-- import           Data.Monoid                    ( Endo )
-- import           Data.Functor.Const             ( Const )
-- import           Data.Foldable                  ( for_ )

-- import qualified Hedgehog                      as H
-- import qualified Prosidy.Test.Gen              as Gen
-- import qualified Hedgehog.Gen                  as HGen

-- test :: TestTree
-- test = testGroup
--     "Optics"
--     [ testGroup
--         "Recurse includes self"
--         [ testProperty "BlockTag" $ prop_recurseSelf Gen.blockTag blockTagsOf
--         , testProperty "InlineTag" $ prop_recurseSelf Gen.inlineTag inlineTagsOf
--         ]
--     , testGroup "Recursive modification is bottom-up"
--                 [testProperty "BlockTag" $
--             -- this test is _expensive_, let's only run it 20 times
--                                            H.withTests 20 prop_rewriteBlock]
--     ]

-- prop_recurseSelf
--     :: ( f x ~ Content t
--        , Show t
--        , Eq t
--        , IsNode t
--        , HasContent t
--        , IsNode x
--        , Traversable f
--        )
--     => H.Gen t
--     -> (forall  n . IsNode n => Optic' (->) (Const (Endo [t])) n t)
--     -> H.Property
-- prop_recurseSelf gen optic = H.property do
--     item <- H.forAll $ HGen.scale (`div` 2) gen
--     let children = item ^.. content . traverse . recurse optic
--         all      = item ^.. recurse optic
--     item : children === all

-- prop_rewriteBlock :: H.Property
-- prop_rewriteBlock = H.property do
--     item <- H.forAll . HGen.scale (`div` 4) $ HGen.filter
--         (\item -> not . null $ item ^.. recurse textOf)
--         Gen.blockTag
--     let modified = rewrite textOf (const "hello world") item
--         oldTexts = item ^.. recurse textOf
--         newTexts = modified ^.. recurse textOf
--     length oldTexts === length newTexts
--     for_ newTexts (=== "hello world")
