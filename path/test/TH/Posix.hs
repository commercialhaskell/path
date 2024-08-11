{-# LANGUAGE CPP #-}

#define PLATFORM_NAME Posix
#include "Include.hs"

qqAbsDir :: FilePath
qqAbsDir = checkInstantiated [absdir|/name/|]

qqAbsFile :: FilePath
qqAbsFile = checkInstantiated [absdir|/name|]

thAbsDir :: FilePath
thAbsDir = checkInstantiated $(mkAbsDir "/name/")

thAbsFile :: FilePath
thAbsFile = checkInstantiated $(mkAbsFile "/name")

liftAbsDir :: FilePath
liftAbsDir = checkInstantiated $(TH.lift (Path "/name/" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = checkInstantiated $(TH.lift (Path "/name" :: Path Abs File))
