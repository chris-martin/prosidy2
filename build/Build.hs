{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Build (main) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import Build.Haskell

main :: IO ()
main = prosidyShake $ do
    registerHaskellRules [ "Prosidy.Manual" .= "prosidy-manual" 
                         , "Prosidy.Markup" .= "prosidy-markup"
                         ]

    "all" ~> do
        need [ "manual"
             , "prosidy-manual"
             , "prosidy-markup"
             ]

    "clean" ~> do
        liftIO $ removeFiles "_out" ["//*"]

    "manual" ~> do
        srcs <- getDirectoryFiles "doc" ["*.pro"]
        res  <- getDirectoryFiles "doc" ["res/*"]
        need $ fmap (\src -> "_out/doc" </> src -<.> ".html") srcs
        need $ fmap ("_out/doc" </>) res

    batch 99 ("_out/doc/*.html" %>)
        (\path -> do 
            let src = dropDirectory1 path -<.> "pro"
            need ["_out/bin/prosidy-manual", src]
            pure src)
        (\srcs ->
            command_ [] "prosidy-manual" ("-o" : "_out" : srcs))

    "_out/doc/res/*" %> \path -> do
        copyFile' (dropDirectory1 path) path

newtype ManualFiles = ManualFiles ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
        
prosidyShake :: Rules () -> IO ()
prosidyShake = shakeArgs shakeOptions


(.=) :: a -> b -> (a, b)
(.=) = (,)