{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prosidy                        ( readDocument, content )
import           Prosidy.Manual                 ( compileDocument, compileToc )
import Prosidy.Manual.TableOfContents (calculateToc, TocItem)
import           Hakyll
import           Hakyll.Contrib.Prosidy
import Control.Lens.Operators

import           Data.Foldable                  ( foldlM
                                                , for_
                                                )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>)
                                                , (-<.>)
                                                )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import           Data.ByteString.Lazy          as LBS
import qualified Data.Text.IO                  as Text.IO

import qualified System.IO                     as IO

import qualified System.FilePath               as Path
import qualified System.Directory              as Dir

import Data.Traversable (sequenceA)
import Data.Maybe (mapMaybe)
import Data.Functor (($>))
import           Control.Monad (unless, void)
import           Control.Monad.Except           ( throwError )

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Internal as Blaze
import Data.Functor (($>))
import qualified Control.Lens as L

main :: IO ()
main = hakyll' $ do
    match "*.pro" $ do
        route (setExtension "html")
        compile $
            prosidyCompiler >>= compileDocumentIO

    match "res/*.css" $ do
        route idRoute
        compile compressCssCompiler

    match "res/*.js" $ do
        route idRoute
        compile copyFileCompiler

    match "res/*.svg" $ do
        route idRoute
        compile copyFileCompiler

hakyll' :: Rules a -> IO ()
hakyll' = hakyllWith $ defaultConfiguration
    { providerDirectory = "./doc"
    }

compileDocumentIO :: Item Document -> Compiler (Item LBS.ByteString)
compileDocumentIO item = do
    saveSnapshot "tableOfContents" compiledTOC
    toc  <- tablesOfContent
    html <- case compileDocument toc item of
        Left err -> throwError [show err]
        Right ok -> pure $ renderHtml ok
    pure $ item $> html
  where
    compiledTOC :: Item (Maybe TocItem)
    compiledTOC
        | itemBody item ^. property "hide" = item $> Nothing
        | otherwise                        = fmap (Just . calculateToc) item

tablesOfContent :: Compiler [Item TocItem]
tablesOfContent = do
    maybeTocs <- loadAllSnapshots "*.pro" "tableOfContents"
    pure $ mapMaybe sequenceA (maybeTocs :: [Item (Maybe TocItem)])