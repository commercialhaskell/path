{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

#define PLATFORM_NAME   Posix
#define IS_WINDOWS      False
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = foo [absdir|/foo/|]

qqAbsFile :: FilePath
qqAbsFile = foo [absdir|/foo|]

thAbsDir :: FilePath
thAbsDir = foo $(mkAbsDir "/foo/")

thAbsFile :: FilePath
thAbsFile = foo $(mkAbsFile "/foo")

liftAbsDir :: FilePath
liftAbsDir = foo $(TH.lift (Path "/foo/" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = foo $(TH.lift (Path "/foo" :: Path Abs File))
