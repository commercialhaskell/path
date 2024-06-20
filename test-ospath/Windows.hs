{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Test suite.

module Windows (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.OsString.Windows as OsString
import Test.Hspec

import Common.Windows (parseFails, parseSucceeds, parserTest)
import qualified Common.Windows
import OsPath.Windows
import OsPath.Internal.Windows
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
  do parseFails [OsString.pstr|..\|]
     parseFails [OsString.pstr|..|]
     parseSucceeds [OsString.pstr|a..|] (Path [OsString.pstr|a..\|])
     parseSucceeds [OsString.pstr|..a|] (Path [OsString.pstr|..a\|])
     parseFails [OsString.pstr|\..|]
     parseFails [OsString.pstr|C:\foo\..\bar\|]
     parseFails [OsString.pstr|C:\foo\bar\..|]

-- | Tests for the tokenizer.
parseAbsDirSpec :: Spec
parseAbsDirSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|.\|]
     failing [OsString.pstr|foo.txt|]
     failing [OsString.pstr|C:|]
     succeeding [OsString.pstr|C:\|] (Path [OsString.pstr|C:\|])
     succeeding [OsString.pstr|C:\\|] (Path [OsString.pstr|C:\|])
     succeeding [OsString.pstr|C:\\\foo\\bar\\mu\|] (Path [OsString.pstr|C:\foo\bar\mu\|])
     succeeding [OsString.pstr|C:\\\foo\\bar\\mu|] (Path [OsString.pstr|C:\foo\bar\mu\|])
     succeeding [OsString.pstr|C:\\\foo\\bar\.\\mu|] (Path [OsString.pstr|C:\foo\bar\mu\|])
     succeeding [OsString.pstr|\\unchost\share|] (Path [OsString.pstr|\\unchost\share\|])
     succeeding [OsString.pstr|\/unchost\share|] (Path [OsString.pstr|\\unchost\share\|])
     succeeding [OsString.pstr|\\unchost\share\\folder\|] (Path [OsString.pstr|\\unchost\share\folder\|])
     succeeding [OsString.pstr|\\?\C:\|] (Path [OsString.pstr|\\?\C:\|])
     succeeding [OsString.pstr|/\?\C:\|] (Path [OsString.pstr|\\?\C:\|])
     succeeding [OsString.pstr|\\?\C:\\\folder\\|] (Path [OsString.pstr|\\?\C:\folder\|])

  where failing x = parserTest parseAbsDir x Nothing
        succeeding x with = parserTest parseAbsDir x (Just with)

-- | Tests for the tokenizer.
parseRelDirSpec :: Spec
parseRelDirSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|/|]
     failing [OsString.pstr|//|]
     failing [OsString.pstr|\|]
     failing [OsString.pstr|\\|]
     failing [OsString.pstr|\\\foo\\bar\\mu\|]
     failing [OsString.pstr|\\\foo\\bar\\\\mu|]
     failing [OsString.pstr|\\\foo\\bar\.\\mu|]
     failing [OsString.pstr|\\unchost\share|]
     failing [OsString.pstr|\\?\C:\|]
     succeeding [OsString.pstr|.\|] (Path [OsString.pstr||])
     succeeding [OsString.pstr|.\.\|] (Path [OsString.pstr||])
     succeeding [OsString.pstr|...|] (Path [OsString.pstr|...\|])
     succeeding [OsString.pstr|foo.bak|] (Path [OsString.pstr|foo.bak\|])
     succeeding [OsString.pstr|.\foo|] (Path [OsString.pstr|foo\|])
     succeeding [OsString.pstr|.\.\foo|] (Path [OsString.pstr|foo\|])
     succeeding [OsString.pstr|.\foo\.\bar|] (Path [OsString.pstr|foo\bar\|])
     succeeding [OsString.pstr|foo\\bar\\mu\\|] (Path [OsString.pstr|foo\bar\mu\|])
     succeeding [OsString.pstr|foo\\bar////mu|] (Path [OsString.pstr|foo\bar\mu\|])
     succeeding [OsString.pstr|foo\\bar\.\\mu|] (Path [OsString.pstr|foo\bar\mu\|])

  where failing x = parserTest parseRelDir x Nothing
        succeeding x with = parserTest parseRelDir x (Just with)

-- | Tests for the tokenizer.
parseAbsFileSpec :: Spec
parseAbsFileSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|.\|]
     failing [OsString.pstr|\.|]
     failing [OsString.pstr|\foo\bar\.|]
     failing [OsString.pstr|~\|]
     failing [OsString.pstr|.\foo.txt|]
     failing [OsString.pstr|\|]
     failing [OsString.pstr|\\|]
     failing [OsString.pstr|\\\foo\\bar\\mu\|]
     failing [OsString.pstr|\...|]
     failing [OsString.pstr|\foo.txt|]
     succeeding [OsString.pstr|C:\\\foo\\bar\\\\mu.txt|] (Path [OsString.pstr|C:\foo\bar\mu.txt|])
     succeeding [OsString.pstr|C:\\\foo\\bar\.\\mu.txt|] (Path [OsString.pstr|C:\foo\bar\mu.txt|])
     succeeding [OsString.pstr|\\unchost\share\\file.txt|] (Path [OsString.pstr|\\unchost\share\file.txt|])
     succeeding [OsString.pstr|\/unchost\share\\file.txt|] (Path [OsString.pstr|\\unchost\share\file.txt|])
     succeeding [OsString.pstr|\\unchost\share\.\folder\\\file.txt|] (Path [OsString.pstr|\\unchost\share\folder\file.txt|])
     succeeding [OsString.pstr|\\?\C:\file.txt|] (Path [OsString.pstr|\\?\C:\file.txt|])
     succeeding [OsString.pstr|/\?\C:\file.txt|] (Path [OsString.pstr|\\?\C:\file.txt|])
     succeeding [OsString.pstr|\\?\C:\\\folder\.\\file.txt|] (Path [OsString.pstr|\\?\C:\folder\file.txt|])

  where failing x = parserTest parseAbsFile x Nothing
        succeeding x with = parserTest parseAbsFile x (Just with)

-- | Tests for the tokenizer.
parseRelFileSpec :: Spec
parseRelFileSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|\|]
     failing [OsString.pstr|\\|]
     failing [OsString.pstr|~\|]
     failing [OsString.pstr|\|]
     failing [OsString.pstr|.\|]
     failing [OsString.pstr|a\.|]
     failing [OsString.pstr|a\..\b|]
     failing [OsString.pstr|a\..|]
     failing [OsString.pstr|..\foo.txt|]
     failing [OsString.pstr|\\|]
     failing [OsString.pstr|\\\foo\\bar\\mu\|]
     failing [OsString.pstr|\\\foo\\bar\\\\mu|]
     failing [OsString.pstr|\\\foo\\bar\.\\mu|]
     failing [OsString.pstr|\\unchost\share\\file.txt|]
     failing [OsString.pstr|\\?\C:\file.txt|]
     succeeding [OsString.pstr|a..|] (Path [OsString.pstr|a..|])
     succeeding [OsString.pstr|...|] (Path [OsString.pstr|...|])
     succeeding [OsString.pstr|foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|.\foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|.\.\foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|.\foo\.\bar.txt|] (Path [OsString.pstr|foo\bar.txt|])
     succeeding [OsString.pstr|foo\\bar\\mu.txt|] (Path [OsString.pstr|foo\bar\mu.txt|])
     succeeding [OsString.pstr|foo\\bar\\\\mu.txt|] (Path [OsString.pstr|foo\bar\mu.txt|])
     succeeding [OsString.pstr|foo\\bar\.\\mu.txt|] (Path [OsString.pstr|foo\bar\mu.txt|])

  where failing x = parserTest parseRelFile x Nothing
        succeeding x with = parserTest parseRelFile x (Just with)

-- | Tests for the 'ToJSON' and 'FromJSON' instances
--
-- Can't use overloaded strings due to some weird issue with bytestring-0.9.2.1 / ghc-7.4.2:
-- https://travis-ci.org/sjakobi/path/jobs/138399072#L989
aesonInstances :: Spec
aesonInstances =
  do it "Decoding \"[\"C:\\\\foo\\\\bar\"]\" as a [Path Abs Dir] should succeed." $
       eitherDecode (LBS.pack "[\"C:\\\\foo\\\\bar\"]") `shouldBe` Right [Path [OsString.pstr|C:\foo\bar\|] :: Path Abs Dir]
     it "Decoding \"[\"C:\\foo\\bar\"]\" as a [Path Rel Dir] should fail." $
       decode (LBS.pack "[\"C:\\foo\\bar\"]") `shouldBe` (Nothing :: Maybe [Path Rel Dir])
     it "Encoding \"[\"C:\\foo\\bar\\mu.txt\"]\" should succeed." $
       encode [Path [OsString.pstr|C:\foo\bar\mu.txt|] :: Path Abs File] `shouldBe` (LBS.pack "[\"C:\\\\foo\\\\bar\\\\mu.txt\"]")

-- | Test QuasiQuoters. Make sure they work the same as the $(mk*) constructors.
quasiquotes :: Spec
quasiquotes =
  do it "[absdir|C:\\|] == $(mkAbsDir \"C:\\\")"
       ([absdir|C:\|] `shouldBe` $(mkAbsDir [OsString.pstr|C:\|]))
     it "[absdir|C:\\chris\\|] == $(mkAbsDir \"C:\\chris\\\")"
       ([absdir|C:\chris\|] `shouldBe` $(mkAbsDir [OsString.pstr|C:\chris\|]))
     it "[reldir|foo|] == $(mkRelDir \"foo\")"
       ([reldir|foo|] `shouldBe` $(mkRelDir [OsString.pstr|foo|]))
     it "[reldir|foo\\bar|] == $(mkRelDir \"foo\\bar\")"
       ([reldir|foo\bar|] `shouldBe` $(mkRelDir [OsString.pstr|foo\bar|]))
     it "[absfile|C:\\chris\\foo.txt|] == $(mkAbsFile \"C:\\chris\\foo.txt\")"
       ([absfile|C:\chris\foo.txt|] `shouldBe` $(mkAbsFile [OsString.pstr|C:\chris\foo.txt|]))
     it "[relfile|foo.exe|] == $(mkRelFile \"foo.exe\")"
       ([relfile|foo.exe|] `shouldBe` $(mkRelFile [OsString.pstr|foo.exe|]))
     it "[relfile|chris\\foo.txt|] == $(mkRelFile \"chris\\foo.txt\")"
       ([relfile|chris\foo.txt|] `shouldBe` $(mkRelFile [OsString.pstr|chris\foo.txt|]))
