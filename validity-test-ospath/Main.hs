-- | Test suite.
module Main (main) where

import Data.Maybe
import Path
import Path.Internal
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import qualified Posix
import qualified Windows

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec =
  modifyMaxShrinks (const 100) $
  parallel $ do
    Posix.spec
    -- See https://github.com/commercialhaskell/path/issues/74
    -- Windows.spec
