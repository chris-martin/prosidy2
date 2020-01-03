module Main
    ( main
    )
where

import           Test.Tasty
import qualified Prosidy.Test.Golden           as Golden
import qualified Prosidy.Test.Optics           as Optics
import qualified Prosidy.Test.Serde            as Serde

main :: IO ()
main = do
    golden <- Golden.test
    let optics = Optics.test
        serde  = Serde.test
    defaultMain $ testGroup "Tests" [golden, optics, serde]
