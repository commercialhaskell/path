{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Test suite.

module Windows (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Test.Hspec

import Common.Windows (parseFails, parseSucceeds, parserTest)
import qualified Common.Windows
import Path.Windows
import Path.Internal.Windows
import TH.Windows ()

-- | Test suite (Windows version).
spec :: Spec
spec =
  do describe "Parsing: Path Abs Dir" parseAbsDirSpec
     describe "Parsing: Path Rel Dir" parseRelDirSpec
     describe "Parsing: Path Abs File" parseAbsFileSpec
     describe "Parsing: Path Rel File" parseRelFileSpec
     Common.Windows.spec
     describe "Restrictions" restrictions
     describe "Aeson Instances" aesonInstances
     describe "QuasiQuotes" quasiquotes

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do parseFails "..\\"
     parseFails ".."
     parseSucceeds "a.." (Path "a..\\")
     parseSucceeds "..a" (Path "..a\\")
     parseFails "\\.."
     parseFails "C:\\foo\\..\\bar\\"
     parseFails "C:\\foo\\bar\\.."

-- | Tests for the tokenizer.
parseAbsDirSpec :: Spec
parseAbsDirSpec =
  do failing ""
     failing ".\\"
     failing "foo.txt"
     failing "C:"
     succeeding "C:\\" (Path "C:\\")
     succeeding "C:\\\\" (Path "C:\\")
     succeeding "C:\\\\\\foo\\\\bar\\\\mu\\" (Path "C:\\foo\\bar\\mu\\")
     succeeding "C:\\\\\\foo\\\\bar\\\\mu" (Path "C:\\foo\\bar\\mu\\")
     succeeding "C:\\\\\\foo\\\\bar\\.\\\\mu" (Path "C:\\foo\\bar\\mu\\")
     succeeding "\\\\unchost\\share" (Path "\\\\unchost\\share\\")
     succeeding "\\/unchost\\share" (Path "\\\\unchost\\share\\")
     succeeding "\\\\unchost\\share\\\\folder\\" (Path "\\\\unchost\\share\\folder\\")
     succeeding "\\\\?\\C:\\" (Path "\\\\?\\C:\\")
     succeeding "/\\?\\C:\\" (Path "\\\\?\\C:\\")
     succeeding "\\\\?\\C:\\\\\\folder\\\\" (Path "\\\\?\\C:\\folder\\")

  where failing x = parserTest parseAbsDir x Nothing
        succeeding x with = parserTest parseAbsDir x (Just with)

-- | Tests for the tokenizer.
parseRelDirSpec :: Spec
parseRelDirSpec =
  do failing ""
     failing "/"
     failing "//"
     failing "\\"
     failing "\\\\"
     failing "\\\\\\foo\\\\bar\\\\mu\\"
     failing "\\\\\\foo\\\\bar\\\\\\\\mu"
     failing "\\\\\\foo\\\\bar\\.\\\\mu"
     failing "\\\\unchost\\share"
     failing "\\\\?\\C:\\"
     succeeding ".\\" (Path "")
     succeeding ".\\.\\" (Path "")
     succeeding "..." (Path "...\\")
     succeeding "foo.bak" (Path "foo.bak\\")
     succeeding ".\\foo" (Path "foo\\")
     succeeding ".\\.\\foo" (Path "foo\\")
     succeeding ".\\foo\\.\\bar" (Path "foo\\bar\\")
     succeeding "foo\\\\bar\\\\mu\\\\" (Path "foo\\bar\\mu\\")
     succeeding "foo\\\\bar////mu" (Path "foo\\bar\\mu\\")
     succeeding "foo\\\\bar\\.\\\\mu" (Path "foo\\bar\\mu\\")

  where failing x = parserTest parseRelDir x Nothing
        succeeding x with = parserTest parseRelDir x (Just with)

-- | Tests for the tokenizer.
parseAbsFileSpec :: Spec
parseAbsFileSpec =
  do failing ""
     failing ".\\"
     failing "\\."
     failing "\\foo\\bar\\."
     failing "~\\"
     failing ".\\foo.txt"
     failing "\\"
     failing "\\\\"
     failing "\\\\\\foo\\\\bar\\\\mu\\"
     failing "\\..."
     failing "\\foo.txt"
     succeeding "C:\\\\\\foo\\\\bar\\\\\\\\mu.txt" (Path "C:\\foo\\bar\\mu.txt")
     succeeding "C:\\\\\\foo\\\\bar\\.\\\\mu.txt" (Path "C:\\foo\\bar\\mu.txt")
     succeeding "\\\\unchost\\share\\\\file.txt" (Path "\\\\unchost\\share\\file.txt")
     succeeding "\\/unchost\\share\\\\file.txt" (Path "\\\\unchost\\share\\file.txt")
     succeeding "\\\\unchost\\share\\.\\folder\\\\\\file.txt" (Path "\\\\unchost\\share\\folder\\file.txt")
     succeeding "\\\\?\\C:\\file.txt" (Path "\\\\?\\C:\\file.txt")
     succeeding "/\\?\\C:\\file.txt" (Path "\\\\?\\C:\\file.txt")
     succeeding "\\\\?\\C:\\\\\\folder\\.\\\\file.txt" (Path "\\\\?\\C:\\folder\\file.txt")

  where failing x = parserTest parseAbsFile x Nothing
        succeeding x with = parserTest parseAbsFile x (Just with)

-- | Tests for the tokenizer.
parseRelFileSpec :: Spec
parseRelFileSpec =
  do failing ""
     failing "\\"
     failing "\\\\"
     failing "~\\"
     failing "\\"
     failing ".\\"
     failing "a\\."
     failing "a\\..\\b"
     failing "a\\.."
     failing "..\\foo.txt"
     failing "\\\\"
     failing "\\\\\\foo\\\\bar\\\\mu\\"
     failing "\\\\\\foo\\\\bar\\\\\\\\mu"
     failing "\\\\\\foo\\\\bar\\.\\\\mu"
     failing "\\\\unchost\\share\\\\file.txt"
     failing "\\\\?\\C:\\file.txt"
     succeeding "a.." (Path "a..")
     succeeding "..." (Path "...")
     succeeding "foo.txt" (Path "foo.txt")
     succeeding ".\\foo.txt" (Path "foo.txt")
     succeeding ".\\.\\foo.txt" (Path "foo.txt")
     succeeding ".\\foo\\.\\bar.txt" (Path "foo\\bar.txt")
     succeeding "foo\\\\bar\\\\mu.txt" (Path "foo\\bar\\mu.txt")
     succeeding "foo\\\\bar\\\\\\\\mu.txt" (Path "foo\\bar\\mu.txt")
     succeeding "foo\\\\bar\\.\\\\mu.txt" (Path "foo\\bar\\mu.txt")

  where failing x = parserTest parseRelFile x Nothing
        succeeding x with = parserTest parseRelFile x (Just with)

-- | Tests for the 'ToJSON' and 'FromJSON' instances
--
-- Can't use overloaded strings due to some weird issue with bytestring-0.9.2.1 / ghc-7.4.2:
-- https://travis-ci.org/sjakobi/path/jobs/138399072#L989
aesonInstances :: Spec
aesonInstances =
  do it "Decoding \"[\"C:\\\\foo\\\\bar\"]\" as a [Path Abs Dir] should succeed." $
       eitherDecode (LBS.pack "[\"C:\\\\foo\\\\bar\"]") `shouldBe` Right [Path "C:\\foo\\bar\\" :: Path Abs Dir]
     it "Decoding \"[\"C:\\foo\\bar\"]\" as a [Path Rel Dir] should fail." $
       decode (LBS.pack "[\"C:\\foo\\bar\"]") `shouldBe` (Nothing :: Maybe [Path Rel Dir])
     it "Encoding \"[\"C:\\foo\\bar\\mu.txt\"]\" should succeed." $
       encode [Path "C:\\foo\\bar\\mu.txt" :: Path Abs File] `shouldBe` LBS.pack "[\"C:\\\\foo\\\\bar\\\\mu.txt\"]"

-- | Test QuasiQuoters. Make sure they work the same as the $(mk*) constructors.
quasiquotes :: Spec
quasiquotes =
  do it "[absdir|C:\\|] == $(mkAbsDir \"C:\\\")"
       ([absdir|C:\|] `shouldBe` $(mkAbsDir "C:\\"))
     it "[absdir|C:\\chris\\|] == $(mkAbsDir \"C:\\chris\\\")"
       ([absdir|C:\chris\|] `shouldBe` $(mkAbsDir "C:\\chris\\"))
     it "[reldir|foo|] == $(mkRelDir \"foo\")"
       ([reldir|foo|] `shouldBe` $(mkRelDir "foo"))
     it "[reldir|foo\\bar|] == $(mkRelDir \"foo\\bar\")"
       ([reldir|foo\bar|] `shouldBe` $(mkRelDir "foo\\bar"))
     it "[absfile|C:\\chris\\foo.txt|] == $(mkAbsFile \"C:\\chris\\foo.txt\")"
       ([absfile|C:\chris\foo.txt|] `shouldBe` $(mkAbsFile "C:\\chris\\foo.txt"))
     it "[relfile|foo.exe|] == $(mkRelFile \"foo.exe\")"
       ([relfile|foo.exe|] `shouldBe` $(mkRelFile "foo.exe"))
     it "[relfile|chris\\foo.txt|] == $(mkRelFile \"chris\\foo.txt\")"
       ([relfile|chris\foo.txt|] `shouldBe` $(mkRelFile "chris\\foo.txt"))
