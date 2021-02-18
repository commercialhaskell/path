{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
#define PLATFORM_NAME   Windows
#define IS_WINDOWS      True
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = foo [absdir|C:\foo\|]

qqAbsFile :: FilePath
qqAbsFile = foo [absdir|C:\foo|]

thAbsDir :: FilePath
thAbsDir = foo $(mkAbsDir "C:\\foo\\")

thAbsFile :: FilePath
thAbsFile = foo $(mkAbsFile "C:\\foo")

liftAbsDir :: FilePath
liftAbsDir = foo $(TH.lift (Path "C:\\foo\\" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = foo $(TH.lift (Path "C:\\foo" :: Path Abs File))

