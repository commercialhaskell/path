{-# LANGUAGE TemplateHaskell #-}

-- | Test suite.

module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Path
import Path.Internal
import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec =
  do describe "Parsing: Path Abs Dir" parseAbsDirSpec
     describe "Parsing: Path Rel Dir" parseRelDirSpec
     describe "Parsing: Path Abs File" parseAbsFileSpec
     describe "Parsing: Path Rel File" parseRelFileSpec
     describe "Operations: (</>)" operationAppend
     describe "Operations: stripDir" operationStripDir
     describe "Operations: isParentOf" operationIsParentOf
     describe "Operations: parent" operationParent
     describe "Operations: filename" operationFilename
     describe "Restrictions" restrictions

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do parseFails "~/"
     parseFails "~/foo"
     parseFails "~/foo/bar"
     parseFails "../"
     parseFails ".."
     parseFails "/.."
     parseFails "/foo/../bar/"
     parseFails "/foo/bar/.."
  where parseFails x =
          it (show x ++ " should be rejected")
             (isNothing (void (parseAbsDir x) <|>
                         void (parseRelDir x) <|>
                         void (parseAbsFile x) <|>
                         void (parseRelDir x)))

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename =
  do it "filename ($(mkAbsDir parent) </> filename $(mkRelFile filename)) == $(mkRelFile filename)"
        (filename ($(mkAbsDir "/home/chris/") </>
                   filename $(mkRelFile "bar.txt")) ==
         $(mkRelFile "bar.txt"))
     it "filename ($(mkRelDir parent) </> filename $(mkRelFile filename)) == $(mkRelFile filename)"
        (filename ($(mkRelDir "home/chris/") </>
                   filename $(mkRelFile "bar.txt")) ==
         $(mkRelFile "bar.txt"))

-- | The 'parent' operation.
operationParent :: Spec
operationParent =
  do it "parent (parent </> child) == parent"
        (parent ($(mkAbsDir "/foo") </>
                    $(mkRelDir "bar")) ==
         $(mkAbsDir "/foo"))
     it "parent \"\" == \"\""
        (parent $(mkAbsDir "/") ==
         $(mkAbsDir "/"))
     it "parent (parent \"\") == \"\""
        (parent (parent $(mkAbsDir "/")) ==
         $(mkAbsDir "/"))

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf =
  do it "isParentOf parent (parent </> child)"
        (isParentOf
           $(mkAbsDir "///bar/")
           ($(mkAbsDir "///bar/") </>
            $(mkRelFile "bar/foo.txt")))
     it "isParentOf parent (parent </> child)"
        (isParentOf
           $(mkRelDir "bar/")
           ($(mkRelDir "bar/") </>
            $(mkRelFile "bob/foo.txt")))

-- | The 'stripDir' operation.
operationStripDir :: Spec
operationStripDir =
  do it "stripDir parent (parent </> child) = child"
        (stripDir $(mkAbsDir "///bar/")
                  ($(mkAbsDir "///bar/") </>
                   $(mkRelFile "bar/foo.txt")) ==
         Just $(mkRelFile "bar/foo.txt"))
     it "stripDir parent (parent </> child) = child"
        (stripDir $(mkRelDir "bar/")
                  ($(mkRelDir "bar/") </>
                   $(mkRelFile "bob/foo.txt")) ==
         Just $(mkRelFile "bob/foo.txt"))
     it "stripDir parent parent = _|_"
        (stripDir $(mkAbsDir "/home/chris/foo")
                  $(mkAbsDir "/home/chris/foo") ==
         Nothing)

-- | The '</>' operation.
operationAppend :: Spec
operationAppend =
  do it "AbsDir + RelDir = AbsDir"
        ($(mkAbsDir "/home/") </>
         $(mkRelDir "chris") ==
         $(mkAbsDir "/home/chris/"))
     it "AbsDir + RelFile = AbsFile"
        ($(mkAbsDir "/home/") </>
         $(mkRelFile "chris/test.txt") ==
         $(mkAbsFile "/home/chris/test.txt"))
     it "RelDir + RelDir = RelDir"
        ($(mkRelDir "home/") </>
         $(mkRelDir "chris") ==
         $(mkRelDir "home/chris"))
     it "RelDir + RelFile = RelFile"
        ($(mkRelDir "home/") </>
         $(mkRelFile "chris/test.txt") ==
         $(mkRelFile "home/chris/test.txt"))

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
