{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prosidy                        ( readDocument, content )
import           Prosidy.Manual                 ( compileDocument, compileToc )
import Prosidy.Manual.TableOfContents (calculateToc)
import           Hakyll
import           Hakyll.Contrib.Prosidy

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

    match "res/*.svg" $ do
        route idRoute
        compile copyFileCompiler

hakyll' :: Rules a -> IO ()
hakyll' = hakyllWith $ defaultConfiguration
    { providerDirectory = "./doc"
    }

compileDocumentIO :: Item Document -> Compiler (Item LBS.ByteString)
compileDocumentIO item = do
    saveSnapshot "tableOfContents" $ fmap calculateToc item
    toc <- loadAllSnapshots "*.pro" "tableOfContents"
    withItemBody (either (throwError . (:[]) . show) (pure . renderHtml) . compileDocument toc) item


--     let doc = itemBody item
--     contents <- saveSnapshot "TOC" $ item $> renderHtml (toc doc)
--     allTOCs  <- foldMap (Blaze.unsafeLazyByteString . itemBody) <$> loadAllSnapshots "*.pro" "TOC"
--     withItemBody (either (throwError . (: []) . show) (pure . renderHtml) . compileDocument (H.ol allTOCs)) item

