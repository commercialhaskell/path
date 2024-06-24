{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Posix
#define PLATFORM_PATH PosixPath
#include "Include.hs"

qqAbsDir :: PLATFORM_PATH
qqAbsDir = checkInstantiated [absdir|/name/|]

qqAbsFile :: PLATFORM_PATH
qqAbsFile = checkInstantiated [absdir|/name|]

thAbsDir :: PLATFORM_PATH
thAbsDir = checkInstantiated $(mkAbsDir [OsString.pstr|/name/|])

thAbsFile :: PLATFORM_PATH
thAbsFile = checkInstantiated $(mkAbsFile [OsString.pstr|/name|])

liftAbsDir :: PLATFORM_PATH
liftAbsDir = checkInstantiated $(TH.lift (Path [OsString.pstr|/name/|] :: Path Abs Dir))

liftAbsFile :: PLATFORM_PATH
liftAbsFile = checkInstantiated $(TH.lift (Path [OsString.pstr|/name|] :: Path Abs File))
