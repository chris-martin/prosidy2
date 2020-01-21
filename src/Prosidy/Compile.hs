{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Prosidy.Compile
    ( Desc
    , Product
    , ProductRule
    , Sum
    , SumRule
    , CompileError(..)
    , SpecError(..)
    , EvalError(..)
    , compile
    , compileM
    , compileRegistry
    , document
    , block
    , inline
    , paragraph
    , break
    , tag
    , literal
    , fragment
    , disallow
    , oneOf
    , child
    , children
    , access
    , embed
    , descendWith
    , req
    , reqText
    , reqParse
    , opt
    , optText
    , optParse
    , prop
    , hoist
    )
where

import           Prelude                 hiding ( break )

import qualified Prosidy                       as P
import           Prosidy.Compile.Internal.Spec  ( SpecError(..) )
import           Prosidy.Compile.Internal.Eval  ( Eval(Eval)
                                                , EvalError(..)
                                                , exhaustive
                                                )
import           Prosidy.Compile.Internal.Compile
import           Prosidy.Compile.Internal.Info  ( infoKey )
import qualified Prosidy.Compile.Internal.Error
                                               as Error
import           Data.Sequence                  ( Seq )
import           Type.Reflection                ( Typeable )
import           Data.Text                      ( Text )
import           Text.Read                      ( readEither )
import qualified Data.Text                     as Text
import           Control.Lens                   ( view )
import           Control.Monad.Morph            ( hoist )

-------------------------------------------------------------------------------
document
    :: Monad context
    => Desc P.Document context output
    -> Product P.Document context output
document desc = productRule (infoKey @P.Document "<DOCUMENT>")
                            "Top-level document."
                            (desc <* Desc (pure exhaustive))

block
    :: Monad context
    => Text
    -> Text
    -> ProductRule P.BlockTag context output
    -> ProductRule P.LiteralTag context output
    -> ProductRule P.Paragraph context output
    -> Product P.Block context output
block name description onTag onLit onPg = do
    onTagRule <- sumRule P._BlockTag (subkey "tag") onTag
    onLitRule <- sumRule P._BlockLiteral (subkey "literal") onLit
    onPgRule  <- sumRule P._BlockParagraph (subkey "paragraph") onPg
    annotateProduct
        <$> chooseOne key description [onTagRule, onLitRule, onPgRule]
  where
    key = infoKey @P.Block name
    subkey x = infoKey @P.Block $ mconcat [name, "-", x]

inline
    :: Monad context
    => Text
    -> Text
    -> ProductRule P.InlineTag context output
    -> ProductRule P.Fragment context output
    -> ProductRule () context output
    -> Product P.Inline context output
inline name description onTag onText onBreak = do
    onTagRule   <- sumRule P._InlineTag (subkey "tag") onTag
    onTextRule  <- sumRule P._InlineFragment (subkey "fragment") onText
    onBreakRule <- sumRule P._Break (subkey "break") onBreak
    annotateProduct
        <$> chooseOne key description [onTagRule, onTextRule, onBreakRule]
  where
    key = infoKey @P.Inline name
    subkey x = infoKey @P.Inline $ mconcat [name, "-", x]

paragraph
    :: forall output context
     . (Monad context)
    => Text
    -> Text
    -> Desc P.Paragraph context output
    -> Product P.Paragraph context output
paragraph name desc = fmap annotateProduct . productRule key desc
    where key = infoKey @P.Paragraph name

break
    :: forall output context
     . Monad context
    => Text
    -> Text
    -> context output
    -> Product () context output
break name description replace = productRule key description (embed replace)
    where key = infoKey @() name

tag
    :: forall input output context
     . (Typeable input, Monad context)
    => P.Key
    -> Text
    -> Desc (P.Region input) context output
    -> Sum (P.Tagged input) context output
tag tagKey description desc = do
    prule <- productRule pKey description (desc <* Desc (pure exhaustive))
    sumRule (P._Tagged tagKey) sKey (annotateProduct prule)
  where
    keyName = P.keyName tagKey
    sKey    = infoKey @(P.Tagged input) keyName
    pKey    = infoKey @(P.Region input) keyName

literal
    :: forall output context
     . Monad context
    => Text
    -> Text
    -> (Text -> context output)
    -> Product P.Literal context output
literal name description transform =
    fmap annotateProduct <$> productRule key description $ access
        (transform . view P.content)
    where key = infoKey @P.Literal name

fragment
    :: forall output context
     . Monad context
    => Text
    -> Text
    -> (Text -> context output)
    -> Product P.Fragment context output
fragment name description transform =
    fmap annotateProduct <$> productRule key description $ access
        (transform . view P.content)
    where key = infoKey @Text name

oneOf
    :: forall input output context
     . (Typeable input, Monad context, P.FromSource input)
    => Text
    -> Text
    -> [SumRule input context output]
    -> Product input context output
oneOf name desc = fmap annotateProduct . chooseOne (infoKey @input name) desc

-------------------------------------------------------------------------------
child
    :: (Monad context, P.FromSource input, P.HasContent input)
    => ProductRule (P.Content input) context output
    -> Desc input context output
child = descendWith P.content

children
    :: ( Traversable t
       , Monoid output
       , Monad context
       , P.FromSource input
       , P.HasContent input
       , P.Content input ~ t x
       )
    => ProductRule x context output
    -> Desc input context output
children = descendWith (P.content . traverse)

-------------------------------------------------------------------------------
req
    :: forall output input context
     . (P.HasMetadata input, Monad context, Typeable output, Read output)
    => P.Key
    -> Text
    -> Desc input context output
req = reqParse (readEither . Text.unpack)

reqText
    :: forall input context
     . (P.HasMetadata input, Monad context)
    => P.Key
    -> Text
    -> Desc input context Text
reqText = reqParse Right

opt
    :: forall output input context
     . (P.HasMetadata input, Monad context, Typeable output, Read output)
    => P.Key
    -> Text
    -> Desc input context (Maybe output)
opt = optParse (readEither . Text.unpack)

optText
    :: forall input context
     . (P.HasMetadata input, Monad context)
    => P.Key
    -> Text
    -> Desc input context (Maybe Text)
optText = optParse Right
