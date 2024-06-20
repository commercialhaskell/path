{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Windows
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = checkInstantiated [absdir|C:\foo\|]

qqAbsFile :: FilePath
qqAbsFile = checkInstantiated [absdir|C:\foo|]

thAbsDir :: FilePath
thAbsDir = checkInstantiated $(mkAbsDir [OsString.pstr|C:\foo\|])

thAbsFile :: FilePath
thAbsFile = checkInstantiated $(mkAbsFile [OsString.pstr|C:\foo|])

liftAbsDir :: FilePath
liftAbsDir = checkInstantiated $(TH.lift (Path [OsString.pstr|C:\foo\|] :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = checkInstantiated $(TH.lift (Path [OsString.pstr|C:\foo|] :: Path Abs File))
