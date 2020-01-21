{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Build.Haskell where

import qualified Text.Regex.PCRE as Regex
import qualified Text.Regex.PCRE.ByteString as Regex
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Array as Array
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.String (IsString(..))
import qualified Data.HashSet as HashSet
import qualified Control.Concurrent as C
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable (for_)
import System.Directory (createDirectoryIfMissing)

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes

newtype Module = Module [Text]
  deriving (Typeable, Eq, Hashable, Binary, NFData)

instance Show Module where
    show (Module txt) = Text.unpack $ Text.intercalate "." txt

instance IsString Module where
    fromString = toModule . Text.pack

newtype IsExe = IsExe Module
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult IsExe = Maybe String

registerHaskellRules :: [(Module, String)] -> Rules ()
registerHaskellRules exePairs = do
    let hsPats = [ "_out/hs//*.o"
                 , "_out/hs//*.hi"
                 , "_out/hs//*.dyn_o"
                 , "_out/hs//*.dyn_hi" 
                 ] 
        exeRules = HashMap.fromList exePairs

    addOracle $ \(IsExe m) -> pure $ HashMap.lookup m exeRules

    for_ exePairs $ \(m, s) -> do
        "_out/bin" </> s %> \_ -> need [moduleObject m]
        s ~> need ["_out/bin" </> s]
    
    hsPats &%> \(obj : _) -> do
        let thisMod = moduleFromOutput obj
            modSrc  = moduleSource thisMod
        deps <- moduleDeps thisMod
        need $ fmap moduleObject deps
        need [modSrc]

        exeName   <- askOracle $ IsExe thisMod
        extraArgs <- case exeName of
            Just n -> do
                liftIO $ createDirectoryIfMissing False "_out/bin"
                pure [ "--make", modSrc
                     , "-main-is", show thisMod ++ ".main"
                     , "-o" , "_out/bin" </> n
                     , "-i", "-isrc"
                     ]
            Nothing -> pure
                [ "-c", modSrc
                , "-i" ,"-i_out/hs"
                ]
    
        command_ [] "ghc" $
            extraArgs ++
            [ "-hidir", "_out/hs"
            , "-odir", "_out/hs"
            , "-split-sections"
            , "-dynamic-too"
            , "-O"
            ]

closureOf :: Module -> Action [FilePath]
closureOf = fmap HashSet.toList . go 
  where
      go m = do
        deps <- moduleDeps m
        subs <- foldMap go deps
        pure $ HashSet.insert (moduleObject m) subs


toModule :: Text -> Module
toModule = Module . Text.split (== '.')

moduleSource :: Module -> FilePath
moduleSource (Module path) = "src" </> Text.unpack (Text.intercalate "/" path) <.> "hs"

moduleObject :: Module -> FilePath
moduleObject (Module path) = "_out/hs" </> Text.unpack (Text.intercalate "/" path) <.> "o"

moduleInterface :: Module -> FilePath
moduleInterface (Module path) = "_out/hs" </> Text.unpack (Text.intercalate "/" path) <.> "hi"

moduleFromOutput :: FilePath -> Module
moduleFromOutput = Module . Text.split (== '/') . Text.pack . dropExtension . dropDirectory1 . dropDirectory1

moduleFromSource :: FilePath -> Module
moduleFromSource = Module . Text.split (== '/') . Text.pack . dropExtension . dropDirectory1 . dropDirectory1

moduleDeps :: Module -> Action [Module]
moduleDeps = unsafePerformIO $ do
    result <- Regex.compile Regex.compExtra 0 "import\\s+(?:qualified\\s+)?(Prosidy[^\\s(]*)"
    regex  <- either (fail . show) pure result
    newCacheIO $ \thisMod -> liftIO $ do
        bytes <- BS.readFile $ moduleSource thisMod
        let matches     = Regex.matchAllText regex bytes
        pure $ toModule . decodeUtf8 . fst . (Array.! 1) <$> matches
{-# NOINLINE moduleDeps #-}

executableModules :: C.MVar (HashMap.HashMap Module String)
executableModules = unsafePerformIO $ C.newMVar mempty

