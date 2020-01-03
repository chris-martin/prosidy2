module Hakyll.Contrib.Prosidy
    ( module Prosidy
    , prosidyCompiler
    )
where

import           Prosidy
import           Hakyll

import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Text.Lazy
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Control.Monad.Error.Class      ( liftEither )
import           Data.Bifunctor                 ( first )

prosidyCompiler :: Compiler (Item Document)
prosidyCompiler = do
    filePath <- getResourceFilePath
    Item iid src <- getResourceText
    doc <- liftEither . first (pure . show) $ parseDocument filePath src
    pure $ Item iid doc

getResourceText :: Compiler (Item Text)
getResourceText = fmap (Text.Lazy.toStrict . decodeUtf8) <$> getResourceLBS
