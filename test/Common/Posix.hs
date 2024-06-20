{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

#define PLATFORM_NAME   Posix
#define IS_WINDOWS      False
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = checkInstantiated [absdir|/foo/|]

qqAbsFile :: FilePath
qqAbsFile = checkInstantiated [absdir|/foo|]

thAbsDir :: FilePath
thAbsDir = checkInstantiated $(mkAbsDir "/foo/")

thAbsFile :: FilePath
thAbsFile = checkInstantiated $(mkAbsFile "/foo")

liftAbsDir :: FilePath
liftAbsDir = checkInstantiated $(TH.lift (Path "/foo/" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = checkInstantiated $(TH.lift (Path "/foo" :: Path Abs File))
