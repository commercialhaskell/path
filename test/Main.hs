{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test suite.

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Path
import Path.Internal
import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Path.Gen ()

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
     describe "Aeson Instances" aesonInstances

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do -- These ~ related ones below are now lifted:
     -- https://github.com/chrisdone/path/issues/19
     parseSucceeds "~/" (Path "~/")
     parseSucceeds "~/foo" (Path "~/foo/")
     parseSucceeds "~/foo/bar" (Path "~/foo/bar/")
     --
     parseFails "../"
     parseFails ".."
     parseFails "."
     parseFails "/.."
     parseFails "/foo/../bar/"
     parseFails "/foo/bar/.."
  where parseFails x =
          it (show x ++ " should be rejected")
             (isNothing (void (parseAbsDir x) <|>
                         void (parseRelDir x) <|>
                         void (parseAbsFile x) <|>
                         void (parseRelFile x)))
        parseSucceeds x with =
          parserTest parseRelDir x (Just with)

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename =
  do it "filename ($(mkAbsDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename) (unit test)"
          (filename ($(mkAbsDir "/home/chris/") </>
                             $(mkRelFile "bar.txt")) ==
                                      filename $(mkRelFile "bar.txt"))

     it "filename ($(mkAbsDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
             forAll genValid $ \(parent :: Path Abs Dir) ->
                         forAll genValid $ \file ->
                                         filename (parent </> file) `shouldBe` filename file

     it "filename ($(mkRelDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename) (unit test)"
             (filename ($(mkRelDir "home/chris/") </>
                                $(mkRelFile "bar.txt")) ==
                                         filename $(mkRelFile "bar.txt"))

     it "filename ($(mkRelDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename)" $
             forAll genValid $ \(parent :: Path Rel Dir) ->
                         forAll genValid $ \file ->
                         filename (parent </> file) `shouldBe` filename file

     it "produces a valid path on when passed a valid absolute path" $ do
        producesValidsOnValids (filename :: Path Abs File -> Path Rel File)

     it "produces a valid path on when passed a valid relative path" $ do
        producesValidsOnValids (filename :: Path Rel File -> Path Rel File)

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

     it "produces a valid path on when passed a valid file path" $ do
        producesValidsOnValids (parent :: Path Abs File -> Path Abs Dir)

     it "produces a valid path on when passed a valid directory path" $ do
        producesValidsOnValids (parent :: Path Abs Dir -> Path Abs Dir)

-- | The 'isParentOf' operation.
operationIsParentOf :: Spec
operationIsParentOf =
  do it "isParentOf parent (parent </> child) (unit test)"
        (isParentOf
           $(mkAbsDir "///bar/")
           ($(mkAbsDir "///bar/") </>
            $(mkRelFile "bar/foo.txt")))
            
     it "isParentOf parent (parent </> child) (unit test)"
        (isParentOf
           $(mkRelDir "bar/")
           ($(mkRelDir "bar/") </>
            $(mkRelFile "bob/foo.txt")))

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Abs Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel File) ->
                isParentOf parent (parent </> child)

     it "isParentOf parent (parent </> child)" $
        forAll genValid $ \(parent :: Path Rel Dir) ->
            forAll genValid $ \(child :: Path Rel Dir) ->
                isParentOf parent (parent </> child)

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

     it "produces a valid path on when passed a valid absolute file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs File -> Maybe (Path Rel File))

     it "produces a valid path on when passed a valid absolute directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Abs Dir -> Path Abs Dir -> Maybe (Path Rel Dir))

     it "produces a valid path on when passed a valid relative file paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel File -> Maybe (Path Rel File))

     it "produces a valid path on when passed a valid relative directory paths" $ do
        producesValidsOnValids2 (stripDir :: Path Rel Dir -> Path Rel Dir -> Maybe (Path Rel Dir))

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

     it "produces a valid path on when creating valid absolute file paths" $ do
        producesValidsOnValids2 ((</>) :: Path Abs Dir -> Path Rel File -> Path Abs File)

     it "produces a valid path on when creating valid absolute directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path Abs Dir -> Path Rel Dir -> Path Abs Dir)

     it "produces a valid path on when creating valid relative file paths" $ do
        producesValidsOnValids2 ((</>) :: Path Rel Dir -> Path Rel File -> Path Rel File)

     it "produces a valid path on when creating valid relative directory paths" $ do
        producesValidsOnValids2 ((</>) :: Path Rel Dir -> Path Rel Dir -> Path Rel Dir)

-- | Tests for the tokenizer.
parseAbsDirSpec :: Spec
parseAbsDirSpec =
  do failing ""
     failing "./"
     failing "foo.txt"
     succeeding "/" (Path "/")
     succeeding "//" (Path "/")
     succeeding "///foo//bar//mu/" (Path "/foo/bar/mu/")
     succeeding "///foo//bar////mu" (Path "/foo/bar/mu/")
     succeeding "///foo//bar/.//mu" (Path "/foo/bar/mu/")

     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseAbsDir :: FilePath -> Maybe (Path Abs Dir))
  where failing x = parserTest parseAbsDir x Nothing
        succeeding x with = parserTest parseAbsDir x (Just with)

-- | Tests for the tokenizer.
parseRelDirSpec :: Spec
parseRelDirSpec =
  do failing ""
     failing "/"
     failing "//"
     succeeding "~/" (Path "~/") -- https://github.com/chrisdone/path/issues/19
     failing "/"
     failing "./"
     failing "././"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "..." (Path ".../")
     succeeding "foo.bak" (Path "foo.bak/")
     succeeding "./foo" (Path "foo/")
     succeeding "././foo" (Path "foo/")
     succeeding "./foo/./bar" (Path "foo/bar/")
     succeeding "foo//bar//mu//" (Path "foo/bar/mu/")
     succeeding "foo//bar////mu" (Path "foo/bar/mu/")
     succeeding "foo//bar/.//mu" (Path "foo/bar/mu/")

     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseRelDir :: FilePath -> Maybe (Path Rel Dir))
  where failing x = parserTest parseRelDir x Nothing
        succeeding x with = parserTest parseRelDir x (Just with)

-- | Tests for the tokenizer.
parseAbsFileSpec :: Spec
parseAbsFileSpec =
  do failing ""
     failing "./"
     failing "/."
     failing "/foo/bar/."
     failing "~/"
     failing "./foo.txt"
     failing "/"
     failing "//"
     failing "///foo//bar//mu/"
     succeeding "/..." (Path "/...")
     succeeding "/foo.txt" (Path "/foo.txt")
     succeeding "///foo//bar////mu.txt" (Path "/foo/bar/mu.txt")
     succeeding "///foo//bar/.//mu.txt" (Path "/foo/bar/mu.txt")

     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseAbsFile :: FilePath -> Maybe (Path Abs File))
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
     failing "a/."
     failing "a/../b"
     failing "a/.."
     failing "../foo.txt"
     failing "//"
     failing "///foo//bar//mu/"
     failing "///foo//bar////mu"
     failing "///foo//bar/.//mu"
     succeeding "a.." (Path "a..")
     succeeding "..." (Path "...")
     succeeding "foo.txt" (Path "foo.txt")
     succeeding "./foo.txt" (Path "foo.txt")
     succeeding "././foo.txt" (Path "foo.txt")
     succeeding "./foo/./bar.txt" (Path "foo/bar.txt")
     succeeding "foo//bar//mu.txt" (Path "foo/bar/mu.txt")
     succeeding "foo//bar////mu.txt" (Path "foo/bar/mu.txt")
     succeeding "foo//bar/.//mu.txt" (Path "foo/bar/mu.txt")

     it "Produces valid paths when it succeeds" $
       validIfSucceedsOnArbitrary
         (parseRelFile :: FilePath -> Maybe (Path Rel File))
  where failing x = parserTest parseRelFile x Nothing
        succeeding x with = parserTest parseRelFile x (Just with)

-- | Parser test.
parserTest :: (Show a1,Show a,Eq a1)
           => (a -> Maybe a1) -> a -> Maybe a1 -> SpecWith ()
parserTest parser input expected =
  it ((case expected of
         Nothing -> "Failing: "
         Just{} -> "Succeeding: ") ++
      "Parsing " ++
      show input ++
      " " ++
      case expected of
        Nothing -> "should fail."
        Just x -> "should succeed with: " ++ show x)
     (actual `shouldBe` expected)
  where actual = parser input

-- | Tests for the 'ToJSON' and 'FromJSON' instances
--
-- Can't use overloaded strings due to some weird issue with bytestring-0.9.2.1 / ghc-7.4.2:
-- https://travis-ci.org/sjakobi/path/jobs/138399072#L989
aesonInstances :: Spec
aesonInstances =
  do it "Decoding \"[\"/foo/bar\"]\" as a [Path Abs Dir] should succeed." $
       eitherDecode (LBS.pack "[\"/foo/bar\"]") `shouldBe` Right [Path "/foo/bar/" :: Path Abs Dir]
     it "Decoding \"[\"/foo/bar\"]\" as a [Path Rel Dir] should fail." $
       decode (LBS.pack "[\"/foo/bar\"]") `shouldBe` (Nothing :: Maybe [Path Rel Dir])
     it "Encoding \"[\"/foo/bar/mu.txt\"]\" should succeed." $
       encode [Path "/foo/bar/mu.txt" :: Path Abs File] `shouldBe` (LBS.pack "[\"/foo/bar/mu.txt\"]")
