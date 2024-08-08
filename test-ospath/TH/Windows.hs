{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Windows
#define PLATFORM_PATH WindowsPath
#include "Include.hs"

qqAbsDir :: PLATFORM_PATH
qqAbsDir = checkInstantiated [absdir|C:\foo\|]

qqAbsFile :: PLATFORM_PATH
qqAbsFile = checkInstantiated [absdir|C:\foo|]

thAbsDir :: PLATFORM_PATH
thAbsDir = checkInstantiated $(mkAbsDir [OsString.pstr|C:\foo\|])

thAbsFile :: PLATFORM_PATH
thAbsFile = checkInstantiated $(mkAbsFile [OsString.pstr|C:\foo|])

liftAbsDir :: PLATFORM_PATH
liftAbsDir = checkInstantiated $(TH.lift (Path [OsString.pstr|C:\foo\|] :: Path Abs Dir))

liftAbsFile :: PLATFORM_PATH
liftAbsFile = checkInstantiated $(TH.lift (Path [OsString.pstr|C:\foo|] :: Path Abs File))
