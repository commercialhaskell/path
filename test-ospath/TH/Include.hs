-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Test functions to check the template haskell bits.
module TH.PLATFORM_NAME where

import qualified Language.Haskell.TH.Syntax as TH
import qualified System.OsString.PLATFORM_NAME as OsString

import OsPath.Internal.PLATFORM_NAME
import OsPath.PLATFORM_NAME

-- | This is a helper type class that checks that splices produce a 'Path' with
--   all type variables instantiated to a type.
--   This ensures that bugs like https://github.com/commercialhaskell/path/issues/159
--   cannot happen.
class CheckInstantiated a b where
    checkInstantiated :: Path a b -> FilePath
    checkInstantiated = toFilePath

instance CheckInstantiated Abs Dir
instance CheckInstantiated Abs File
instance CheckInstantiated Rel Dir
instance CheckInstantiated Rel File

qqRelDir :: FilePath
qqRelDir = checkInstantiated [reldir|name/|]

qqRelFile :: FilePath
qqRelFile = checkInstantiated [relfile|name|]

thRelDir :: FilePath
thRelDir = checkInstantiated $(mkRelDir [OsString.pstr|name/|])

thRelFile :: FilePath
thRelFile = checkInstantiated $(mkRelFile [OsString.pstr|name|])

liftRelDir :: FilePath
liftRelDir = checkInstantiated $(TH.lift (Path [OsString.pstr|name/|] :: Path Rel Dir))

liftRelFile :: FilePath
liftRelFile = checkInstantiated $(TH.lift (Path [OsString.pstr|name|] :: Path Rel File))
