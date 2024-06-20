{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

#define PLATFORM_NAME   Windows
#define IS_WINDOWS      True
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = checkInstantiated [absdir|C:\foo\|]

qqAbsFile :: FilePath
qqAbsFile = checkInstantiated [absdir|C:\foo|]

thAbsDir :: FilePath
thAbsDir = checkInstantiated $(mkAbsDir "C:\\foo\\")

thAbsFile :: FilePath
thAbsFile = checkInstantiated $(mkAbsFile "C:\\foo")

liftAbsDir :: FilePath
liftAbsDir = checkInstantiated $(TH.lift (Path "C:\\foo\\" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = checkInstantiated $(TH.lift (Path "C:\\foo" :: Path Abs File))

