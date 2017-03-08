{-# LANGUAGE CPP #-}

module Main (main) where

#ifdef mingw32_HOST_OS
import Windows (spec)
#else
import Posix (spec)
#endif

import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec
