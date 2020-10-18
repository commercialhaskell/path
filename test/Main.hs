module Main (main) where

import qualified Windows
import qualified Posix

import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec $ do
    describe "Path.Windows" Windows.spec
    describe "Path.Posix" Posix.spec
