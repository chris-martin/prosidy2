{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prosidy                        ( readDocument )
import           Prosidy.Manual                 ( compileDocument )
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

main :: IO ()
main = hakyll' $ do
    match "*.pro" $ do
        route (setExtension "html")
        compile $ prosidyCompiler >>= withItemBody compileDocumentIO

hakyll' :: Rules a -> IO ()
hakyll' = hakyllWith $ defaultConfiguration { providerDirectory = "./doc" }

compileDocumentIO :: Document -> Compiler LBS.ByteString
compileDocumentIO =
    either (throwError . (: []) . show) (pure . renderHtml) . compileDocument

