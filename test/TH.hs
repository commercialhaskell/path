{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import qualified Language.Haskell.TH.Syntax as TH
import Path.Posix
import Path.Internal.Posix



class Foo a b where
    foo :: Path a b -> FilePath
    foo = toFilePath

instance Foo Abs Dir
instance Foo Abs File
instance Foo Rel Dir
instance Foo Rel File



qqAbsDir :: FilePath
#ifdef mingw32_HOST_OS
qqAbsDir = foo [absdir|C:\foo\|]
#else
qqAbsDir = foo [absdir|/foo/|]
#endif

qqAbsFile :: FilePath
#ifdef mingw32_HOST_OS
qqAbsFile = foo [absdir|C:\foo|]
#else
qqAbsFile = foo [absdir|/foo|]
#endif

qqRelDir :: FilePath
qqRelDir = foo [reldir|foo/|]

qqRelFile :: FilePath
qqRelFile = foo [relfile|foo|]

thAbsDir :: FilePath
#ifdef mingw32_HOST_OS
thAbsDir = foo $(mkAbsDir "C:\\foo\\")
#else
thAbsDir = foo $(mkAbsDir "/foo/")
#endif

thAbsFile :: FilePath
#ifdef mingw32_HOST_OS
thAbsFile = foo $(mkAbsFile "C:\\foo")
#else
thAbsFile = foo $(mkAbsFile "/foo")
#endif

thRelDir :: FilePath
thRelDir = foo $(mkRelDir "foo/")

thRelFile :: FilePath
thRelFile = foo $(mkRelFile "foo")

liftAbsDir :: FilePath
#ifdef mingw32_HOST_OS
liftAbsDir = foo $(TH.lift (Path "C:\\foo\\" :: Path Abs Dir))
#else
liftAbsDir = foo $(TH.lift (Path "/foo/" :: Path Abs Dir))
#endif

liftAbsFile :: FilePath
#ifdef mingw32_HOST_OS
liftAbsFile = foo $(TH.lift (Path "C:\\foo" :: Path Abs File))
#else
liftAbsFile = foo $(TH.lift (Path "/foo" :: Path Abs File))
#endif

liftRelDir :: FilePath
liftRelDir = foo $(TH.lift (Path "foo/" :: Path Rel Dir))

liftRelFile :: FilePath
liftRelFile = foo $(TH.lift (Path "foo" :: Path Rel File))
