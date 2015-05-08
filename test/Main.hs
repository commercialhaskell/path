-- | Test suite.

module Main where

import Path
import Path.Internal

import Data.Monoid
import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "Parsing: Path Abs Dir" parseAbsDirSpec
  describe "Parsing: Path Rel Dir" parseRelDirSpec
  describe "Parsing: Path Abs File" parseAbsFileSpec
  describe "Parsing: Path Rel File" parseRelFileSpec

-- | Tests for the tokenizer.
parseAbsDirSpec :: Spec
parseAbsDirSpec =
  do failing ""
     failing "./"
     failing "~/"
     failing "foo.txt"
     succeeding "/" (Path "/")
     succeeding "//" (Path "/")
     succeeding "///foo//bar//mu/" (Path "/foo/bar/mu/")
     succeeding "///foo//bar////mu" (Path "/foo/bar/mu/")
     succeeding "///foo//bar/.//mu" (Path "/foo/bar/mu/")
  where failing x = parserTest parseAbsDir x Nothing
        succeeding x with = parserTest parseAbsDir x (Just with)

-- | Tests for the tokenizer.
parseRelDirSpec :: Spec
parseRelDirSpec =
  do failing ""
     failing "/"
     failing "//"
     failing "~/"
     failing "/"
     failing "./"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "foo.bak" (Path "foo.bak/")
     succeeding "./foo" (Path "foo/")
     succeeding "foo//bar//mu//" (Path "foo/bar/mu/")
     succeeding "foo//bar////mu" (Path "foo/bar/mu/")
     succeeding "foo//bar/.//mu" (Path "foo/bar/mu/")
  where failing x = parserTest parseRelDir x Nothing
        succeeding x with = parserTest parseRelDir x (Just with)

-- | Tests for the tokenizer.
parseAbsFileSpec :: Spec
parseAbsFileSpec =
  do failing ""
     failing "./"
     failing "~/"
     failing "./foo.txt"
     failing "/"
     failing "//"
     failing "///foo//bar//mu/"
     succeeding "/foo.txt" (Path "/foo.txt")
     succeeding "///foo//bar////mu.txt" (Path "/foo/bar/mu.txt")
     succeeding "///foo//bar/.//mu.txt" (Path "/foo/bar/mu.txt")
  where failing x = parserTest parseAbsFile x Nothing
        succeeding x with = parserTest parseAbsFile x (Just with)

-- | Tests for the tokenizer.
parseRelFileSpec :: Spec
parseRelFileSpec =
  do failing ""
     failing "/"
     failing "//"
     failing "~/"
     failing "/"
     failing "./"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "foo.txt" (Path "foo.txt")
     succeeding "./foo.txt" (Path "foo.txt")
     succeeding "foo//bar//mu.txt" (Path "foo/bar/mu.txt")
     succeeding "foo//bar////mu.txt" (Path "foo/bar/mu.txt")
     succeeding "foo//bar/.//mu.txt" (Path "foo/bar/mu.txt")
  where failing x = parserTest parseRelFile x Nothing
        succeeding x with = parserTest parseRelFile x (Just with)

-- | Parser test.
parserTest :: (Show a1,Show a,Eq a1)
           => (a -> Maybe a1) -> a -> Maybe a1 -> SpecWith ()
parserTest parser input expected =
  it ((case expected of
         Nothing -> "Failing: "
         Just{} -> "Succeeding: ") <>
      "Parsing " <>
      show input <>
      " " <>
      case expected of
        Nothing -> "should fail."
        Just x -> "should succeed with: " <> show x)
     (actual == expected)
  where actual = parser input
