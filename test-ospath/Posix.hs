{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Test suite.

module Posix (spec) where

import Test.Hspec

import Common.Posix (parseFails, parseSucceeds, parserTest)
import qualified Common.Posix
import OsPath.Posix
import OsPath.Internal.Posix
import qualified System.OsString.Compat.Posix as OsString
import TH.Posix ()

-- | Test suite (Posix version).
spec :: Spec
spec =
  do describe "Parsing: Path Abs Dir" parseAbsDirSpec
     describe "Parsing: Path Rel Dir" parseRelDirSpec
     describe "Parsing: Path Abs File" parseAbsFileSpec
     describe "Parsing: Path Rel File" parseRelFileSpec
     Common.Posix.spec
     describe "Restrictions" restrictions
     describe "QuasiQuotes" quasiquotes

-- | Restricting the input of any tricks.
restrictions :: Spec
restrictions =
  do -- These ~ related ones below are now lifted:
     -- https://github.com/chrisdone/path/issues/19
     parseSucceeds [OsString.pstr|~/|] (Path [OsString.pstr|~/|])
     parseSucceeds [OsString.pstr|~/foo|] (Path [OsString.pstr|~/foo/|])
     parseSucceeds [OsString.pstr|~/foo/bar|] (Path [OsString.pstr|~/foo/bar/|])
     parseSucceeds [OsString.pstr|a..|] (Path [OsString.pstr|a../|])
     parseSucceeds [OsString.pstr|..a|] (Path [OsString.pstr|..a/|])
     --
     parseFails [OsString.pstr|../|]
     parseFails [OsString.pstr|..|]
     parseFails [OsString.pstr|/..|]
     parseFails [OsString.pstr|/foo/../bar/|]
     parseFails [OsString.pstr|/foo/bar/..|]

-- | Tests for the tokenizer.
parseAbsDirSpec :: Spec
parseAbsDirSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|./|]
     failing [OsString.pstr|foo.txt|]
     succeeding [OsString.pstr|/|] (Path [OsString.pstr|/|])
     succeeding [OsString.pstr|//|] (Path [OsString.pstr|/|])
     succeeding [OsString.pstr|///foo//bar//mu/|] (Path [OsString.pstr|/foo/bar/mu/|])
     succeeding [OsString.pstr|///foo//bar////mu|] (Path [OsString.pstr|/foo/bar/mu/|])
     succeeding [OsString.pstr|///foo//bar/.//mu|] (Path [OsString.pstr|/foo/bar/mu/|])

  where failing x = parserTest parseAbsDir x Nothing
        succeeding x with = parserTest parseAbsDir x (Just with)

-- | Tests for the tokenizer.
parseRelDirSpec :: Spec
parseRelDirSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|/|]
     failing [OsString.pstr|//|]
     succeeding [OsString.pstr|~/|] (Path [OsString.pstr|~/|]) -- https://github.com/chrisdone/path/issues/19
     failing [OsString.pstr|/|]
     succeeding [OsString.pstr|./|] (Path [OsString.pstr||])
     succeeding [OsString.pstr|././|] (Path [OsString.pstr||])
     failing [OsString.pstr|//|]
     failing [OsString.pstr|///foo//bar//mu/|]
     failing [OsString.pstr|///foo//bar////mu|]
     failing [OsString.pstr|///foo//bar/.//mu|]
     succeeding [OsString.pstr|...|] (Path [OsString.pstr|.../|])
     succeeding [OsString.pstr|foo.bak|] (Path [OsString.pstr|foo.bak/|])
     succeeding [OsString.pstr|./foo|] (Path [OsString.pstr|foo/|])
     succeeding [OsString.pstr|././foo|] (Path [OsString.pstr|foo/|])
     succeeding [OsString.pstr|./foo/./bar|] (Path [OsString.pstr|foo/bar/|])
     succeeding [OsString.pstr|foo//bar//mu//|] (Path [OsString.pstr|foo/bar/mu/|])
     succeeding [OsString.pstr|foo//bar////mu|] (Path [OsString.pstr|foo/bar/mu/|])
     succeeding [OsString.pstr|foo//bar/.//mu|] (Path [OsString.pstr|foo/bar/mu/|])

  where failing x = parserTest parseRelDir x Nothing
        succeeding x with = parserTest parseRelDir x (Just with)

-- | Tests for the tokenizer.
parseAbsFileSpec :: Spec
parseAbsFileSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|./|]
     failing [OsString.pstr|/.|]
     failing [OsString.pstr|/foo/bar/.|]
     failing [OsString.pstr|~/|]
     failing [OsString.pstr|./foo.txt|]
     failing [OsString.pstr|/|]
     failing [OsString.pstr|//|]
     failing [OsString.pstr|///foo//bar//mu/|]
     succeeding [OsString.pstr|/...|] (Path [OsString.pstr|/...|])
     succeeding [OsString.pstr|/foo.txt|] (Path [OsString.pstr|/foo.txt|])
     succeeding [OsString.pstr|///foo//bar////mu.txt|] (Path [OsString.pstr|/foo/bar/mu.txt|])
     succeeding [OsString.pstr|///foo//bar/.//mu.txt|] (Path [OsString.pstr|/foo/bar/mu.txt|])

  where failing x = parserTest parseAbsFile x Nothing
        succeeding x with = parserTest parseAbsFile x (Just with)

-- | Tests for the tokenizer.
parseRelFileSpec :: Spec
parseRelFileSpec =
  do failing [OsString.pstr||]
     failing [OsString.pstr|/|]
     failing [OsString.pstr|//|]
     failing [OsString.pstr|~/|]
     failing [OsString.pstr|/|]
     failing [OsString.pstr|./|]
     failing [OsString.pstr|a/.|]
     failing [OsString.pstr|a/../b|]
     failing [OsString.pstr|a/..|]
     failing [OsString.pstr|../foo.txt|]
     failing [OsString.pstr|//|]
     failing [OsString.pstr|///foo//bar//mu/|]
     failing [OsString.pstr|///foo//bar////mu|]
     failing [OsString.pstr|///foo//bar/.//mu|]
     succeeding [OsString.pstr|a..|] (Path [OsString.pstr|a..|])
     succeeding [OsString.pstr|...|] (Path [OsString.pstr|...|])
     succeeding [OsString.pstr|foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|./foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|././foo.txt|] (Path [OsString.pstr|foo.txt|])
     succeeding [OsString.pstr|./foo/./bar.txt|] (Path [OsString.pstr|foo/bar.txt|])
     succeeding [OsString.pstr|foo//bar//mu.txt|] (Path [OsString.pstr|foo/bar/mu.txt|])
     succeeding [OsString.pstr|foo//bar////mu.txt|] (Path [OsString.pstr|foo/bar/mu.txt|])
     succeeding [OsString.pstr|foo//bar/.//mu.txt|] (Path [OsString.pstr|foo/bar/mu.txt|])

  where failing x = parserTest parseRelFile x Nothing
        succeeding x with = parserTest parseRelFile x (Just with)

-- | Test QuasiQuoters. Make sure they work the same as the $(mk*) constructors.
quasiquotes :: Spec
quasiquotes =
  do it "[absdir|/|] == $(mkAbsDir \"/\")"
       ([absdir|/|] `shouldBe` $(mkAbsDir [OsString.pstr|/|]))
     it "[absdir|/home|] == $(mkAbsDir \"/home\")"
       ([absdir|/home|] `shouldBe` $(mkAbsDir [OsString.pstr|/home|]))
     it "[reldir|foo|] == $(mkRelDir \"foo\")"
       ([reldir|foo|] `shouldBe` $(mkRelDir [OsString.pstr|foo|]))
     it "[reldir|foo/bar|] == $(mkRelDir \"foo/bar\")"
       ([reldir|foo/bar|] `shouldBe` $(mkRelDir [OsString.pstr|foo/bar|]))
     it "[absfile|/home/chris/foo.txt|] == $(mkAbsFile \"/home/chris/foo.txt\")"
       ([absfile|/home/chris/foo.txt|] `shouldBe` $(mkAbsFile [OsString.pstr|/home/chris/foo.txt|]))
     it "[relfile|foo|] == $(mkRelFile \"foo\")"
       ([relfile|foo|] `shouldBe` $(mkRelFile [OsString.pstr|foo|]))
     it "[relfile|chris/foo.txt|] == $(mkRelFile \"chris/foo.txt\")"
       ([relfile|chris/foo.txt|] `shouldBe` $(mkRelFile [OsString.pstr|chris/foo.txt|]))
