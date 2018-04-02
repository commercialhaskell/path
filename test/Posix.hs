{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test suite.

module Posix (spec) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import Path.Posix
import Path.Internal
import Test.Hspec

-- | Test suite (Posix version).
spec :: Spec
spec =
  do describe "Parsing: Path Abs Dir" parseAbsDirSpec
     describe "Parsing: Path Rel Dir" parseRelDirSpec
     describe "Parsing: Path Abs File" parseAbsFileSpec
     describe "Parsing: Path Rel File" parseRelFileSpec
     describe "Operations: (</>)" operationAppend
     describe "Operations: toFilePath" operationToFilePath
     describe "Operations: stripProperPrefix" operationStripProperPrefix
     describe "Operations: isProperPrefixOf" operationIsProperPrefixOf
     describe "Operations: parent" operationParent
     describe "Operations: filename" operationFilename
     describe "Operations: dropExtension" operationDropExtension
     describe "Operations: dirname" operationDirname
     describe "Operations: addFileExtension" operationAddFileExtension
     describe "Operations: setFileExtension" operationSetFileExtension
     describe "Restrictions" restrictions
     describe "Aeson Instances" aesonInstances
     describe "QuasiQuotes" quasiquotes

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do -- These ~ related ones below are now lifted:
     -- https://github.com/chrisdone/path/issues/19
     parseSucceeds "~/" (Path "~/")
     parseSucceeds "~/foo" (Path "~/foo/")
     parseSucceeds "~/foo/bar" (Path "~/foo/bar/")
     parseSucceeds "a.." (Path "a../")
     parseSucceeds "..a" (Path "..a/")
     --
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
                         void (parseRelFile x)))
        parseSucceeds x with =
          parserTest parseRelDir x (Just with)

-- | The 'dirname' operation.
operationDirname :: Spec
operationDirname = do
  it
    "dirname ($(mkAbsDir parent) </> $(mkRelFile dirname)) == dirname $(mkRelFile dirname) (unit test)"
    (dirname ($(mkAbsDir "/home/chris/") </> $(mkRelDir "bar")) ==
     dirname $(mkRelDir "bar"))
  it
    "dirname ($(mkRelDir parent) </> $(mkRelFile dirname)) == dirname $(mkRelFile dirname) (unit test)"
    (dirname ($(mkRelDir "home/chris/") </> $(mkRelDir "bar")) ==
     dirname $(mkRelDir "bar"))
  it
    "dirname / must be a Rel path"
    ((parseAbsDir $ show $ dirname (fromJust (parseAbsDir "/"))
     :: Maybe (Path Abs Dir)) == Nothing)

-- | The 'filename' operation.
operationFilename :: Spec
operationFilename =
  do it "filename ($(mkAbsDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename) (unit test)"
          (filename ($(mkAbsDir "/home/chris/") </>
                             $(mkRelFile "bar.txt")) ==
                                      filename $(mkRelFile "bar.txt"))

     it "filename ($(mkRelDir parent) </> $(mkRelFile filename)) == filename $(mkRelFile filename) (unit test)"
             (filename ($(mkRelDir "home/chris/") </>
                                $(mkRelFile "bar.txt")) ==
                                         filename $(mkRelFile "bar.txt"))

-- | The 'dropExtension' operation.
operationDropExtension :: Spec
operationDropExtension =
  do it "dropExtension ($(mkAbsDir parent) </> $(mkRelFile filename)) == dropExtension $(mkRelFile filename) (unit test)"
          (dropExtension ($(mkAbsDir "/home/chris/") </>
                             $(mkRelFile "bar.txt")) ==
                                      dropExtension $(mkRelFile "bar.txt"))

     it "dropExtension ($(mkRelDir parent) </> $(mkRelFile filename)) == dropExtension $(mkRelFile filename) (unit test)"
             (dropExtension ($(mkRelDir "home/chris/") </>
                                $(mkRelFile "bar.txt")) ==
                                         dropExtension $(mkRelFile "bar.txt"))

     it "dropExtension ('bar.txt.jpg') == 'bar.txt' (unit test)"
             (dropExtension ($(mkRelFile "bar.txt.jpg")) == $(mkRelFile "bar.txt"))

-- | The 'parent' operation.
operationParent :: Spec
operationParent =
  do it "parent (parent </> child) == parent"
        (parent ($(mkAbsDir "/foo") </>
                    $(mkRelDir "bar")) ==
         $(mkAbsDir "/foo"))
     it "parent \"/\" == \"/\""
        (parent $(mkAbsDir "/") == $(mkAbsDir "/"))
     it "parent \"/x\" == \"/\""
        (parent $(mkAbsDir "/x") == $(mkAbsDir "/"))
     it "parent \"x\" == \".\""
        (parent $(mkRelDir "x") == $(mkRelDir "."))
     it "parent \".\" == \".\""
        (parent $(mkRelDir ".") == $(mkRelDir "."))

-- | The 'isProperPrefixOf' operation.
operationIsProperPrefixOf :: Spec
operationIsProperPrefixOf =
  do it "isProperPrefixOf parent (parent </> child) (absolute)"
        (isProperPrefixOf
           $(mkAbsDir "///bar/")
           ($(mkAbsDir "///bar/") </>
            $(mkRelFile "bar/foo.txt")))

     it "isProperPrefixOf parent (parent </> child) (relative)"
        (isProperPrefixOf
           $(mkRelDir "bar/")
           ($(mkRelDir "bar/") </>
            $(mkRelFile "bob/foo.txt")))

     it "not (x `isProperPrefixOf` x)"
        (not (isProperPrefixOf $(mkRelDir "x") $(mkRelDir "x")))

     it "not (/ `isProperPrefixOf` /)"
        (not (isProperPrefixOf $(mkAbsDir "/") $(mkAbsDir "/")))

-- | The 'stripProperPrefix' operation.
operationStripProperPrefix :: Spec
operationStripProperPrefix =
  do it "stripProperPrefix parent (parent </> child) = child (unit test)"
        (stripProperPrefix $(mkAbsDir "///bar/")
                  ($(mkAbsDir "///bar/") </>
                   $(mkRelFile "bar/foo.txt")) ==
         Just $(mkRelFile "bar/foo.txt"))

     it "stripProperPrefix parent (parent </> child) = child (unit test)"
        (stripProperPrefix $(mkRelDir "bar/")
                  ($(mkRelDir "bar/") </>
                   $(mkRelFile "bob/foo.txt")) ==
         Just $(mkRelFile "bob/foo.txt"))

     it "stripProperPrefix parent parent = _|_"
        (stripProperPrefix $(mkAbsDir "/home/chris/foo")
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
     it ". + . = ."
        ($(mkRelDir "./") </> $(mkRelDir ".") == $(mkRelDir "."))
     it ". + x = x"
        ($(mkRelDir ".") </> $(mkRelDir "x") == $(mkRelDir "x"))
     it "x + . = x"
        ($(mkRelDir "x") </> $(mkRelDir "./") == $(mkRelDir "x"))
     it "RelDir + RelFile = RelFile"
        ($(mkRelDir "home/") </>
         $(mkRelFile "chris/test.txt") ==
         $(mkRelFile "home/chris/test.txt"))

operationToFilePath :: Spec
operationToFilePath =
  do it "toFilePath $(mkRelDir \".\") == \"./\""
        (toFilePath $(mkRelDir ".") == "./")
     it "show $(mkRelDir \".\") == \"\\\"./\\\"\""
        (show $(mkRelDir ".") == "\"./\"")

operationAddFileExtension :: Spec
operationAddFileExtension = do
  it "adds extension if there is none" $
    addFileExtension "ext" $(mkAbsFile "/directory/path")
      `shouldReturn` $(mkAbsFile "/directory/path.ext")
  it "adds extension if there is already one" $
    addFileExtension "baz" $(mkRelFile "foo.bar")
      `shouldReturn` $(mkRelFile "foo.bar.baz")
  it "adds extension with dot" $
    addFileExtension ".bar" $(mkRelFile "foo")
      `shouldReturn` $(mkRelFile "foo.bar")
  it "adds extension with dot after dot" $
    $(mkRelFile "foo.") <.> ".bar"
      `shouldReturn` $(mkRelFile "foo..bar")
  it "adds extension without dot after dot" $
    $(mkRelFile "foo.") <.> "bar"
      `shouldReturn` $(mkRelFile "foo..bar")
  it "adds extension with separator" $  -- I'm not sure it's okay
    $(mkRelFile "foo") <.> "evil/extension"
      `shouldReturn` $(mkRelFile "foo.evil/extension")
  it "adds extension to file inside dotted directory" $
    $(mkRelFile "foo.bar/baz") <.> "txt"
      `shouldReturn` $(mkRelFile "foo.bar/baz.txt")
  it "throws InvalidRelFile extension if extenstion ends with /" $
    $(mkRelFile "foo") <.> "evil/"
      `shouldThrow` (== InvalidRelFile "foo.evil/")
  it "throws InvalidAbsFile extension if extenstion ends with /" $
    $(mkAbsFile "/home/cfg") <.> "txt/" -- No Eq instance for PathException
      `shouldThrow` (== InvalidAbsFile "/home/cfg.txt/")

operationSetFileExtension :: Spec
operationSetFileExtension = do
  it "adds extension if there is none" $
    setFileExtension "txt" $(mkRelFile "foo")
      `shouldReturn` $(mkRelFile "foo.txt")
  it "replaces extension if the input path already has one" $
    setFileExtension "txt" $(mkRelFile "foo.bar")
      `shouldReturn` $(mkRelFile "foo.txt")

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
     succeeding "./" (Path "")
     succeeding "././" (Path "")
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

-- | Test QuasiQuoters. Make sure they work the same as the $(mk*) constructors.
quasiquotes :: Spec
quasiquotes =
  do it "[absdir|/|] == $(mkAbsDir \"/\")"
       ([absdir|/|] `shouldBe` $(mkAbsDir "/"))
     it "[absdir|/home|] == $(mkAbsDir \"/home\")"
       ([absdir|/home|] `shouldBe` $(mkAbsDir "/home"))
     it "[reldir|foo|] == $(mkRelDir \"foo\")"
       ([reldir|foo|] `shouldBe` $(mkRelDir "foo"))
     it "[reldir|foo/bar|] == $(mkRelDir \"foo/bar\")"
       ([reldir|foo/bar|] `shouldBe` $(mkRelDir "foo/bar"))
     it "[absfile|/home/chris/foo.txt|] == $(mkAbsFile \"/home/chris/foo.txt\")"
       ([absfile|/home/chris/foo.txt|] `shouldBe` $(mkAbsFile "/home/chris/foo.txt"))
     it "[relfile|foo|] == $(mkRelFile \"foo\")"
       ([relfile|foo|] `shouldBe` $(mkRelFile "foo"))
     it "[relfile|chris/foo.txt|] == $(mkRelFile \"chris/foo.txt\")"
       ([relfile|chris/foo.txt|] `shouldBe` $(mkRelFile "chris/foo.txt"))
