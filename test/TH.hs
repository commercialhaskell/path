{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import qualified Language.Haskell.TH.Syntax as TH
import Path
import Path.Internal



class Foo a b where
    foo :: Path a b -> FilePath
    foo = toFilePath

instance Foo Abs Dir
instance Foo Abs File
instance Foo Rel Dir
instance Foo Rel File



qqAbsDir :: FilePath
qqAbsDir = foo [absdir|/foo/|]

qqAbsFile :: FilePath
qqAbsFile = foo [absfile|/foo|]

qqRelDir :: FilePath
qqRelDir = foo [reldir|foo/|]

qqRelFile :: FilePath
qqRelFile = foo [relfile|foo|]

thAbsDir :: FilePath
thAbsDir = foo $(mkAbsDir "/foo/")

thAbsFile :: FilePath
thAbsFile = foo $(mkAbsFile "/foo")

thRelDir :: FilePath
thRelDir = foo $(mkRelDir "foo/")

thRelFile :: FilePath
thRelFile = foo $(mkRelFile "foo")

liftAbsDir :: FilePath
liftAbsDir = foo $(TH.lift (Path "/foo/" :: Path Abs Dir))

liftAbsFile :: FilePath
liftAbsFile = foo $(TH.lift (Path "/foo" :: Path Abs File))

liftRelDir :: FilePath
liftRelDir = foo $(TH.lift (Path "foo/" :: Path Rel Dir))

liftRelFile :: FilePath
liftRelFile = foo $(TH.lift (Path "foo" :: Path Rel File))
