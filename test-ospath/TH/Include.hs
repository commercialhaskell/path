-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Test functions to check the template haskell bits.
module TH.PLATFORM_NAME where

import Data.Proxy (Proxy)
import qualified Language.Haskell.TH.Syntax as TH
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)

import OsPath.Internal.PLATFORM_NAME
import OsPath.PLATFORM_NAME
import qualified System.OsString.Compat.PLATFORM_NAME as OsString

-- | This is a helper type class that checks that splices produce a 'Path' with
--   all type variables instantiated to a type.
--   This ensures that bugs like https://github.com/commercialhaskell/path/issues/159
--   cannot happen.
class CheckInstantiated a b where
    checkInstantiated :: Path a b -> PLATFORM_PATH
    checkInstantiated = toOsPath

instance CheckInstantiated Abs Dir
instance CheckInstantiated Abs File
instance CheckInstantiated Rel Dir
instance CheckInstantiated Rel File

qqRelDir :: PLATFORM_PATH
qqRelDir = checkInstantiated [reldir|name/|]

qqRelFile :: PLATFORM_PATH
qqRelFile = checkInstantiated [relfile|name|]

thRelDir :: PLATFORM_PATH
thRelDir = checkInstantiated $(mkRelDir [OsString.pstr|name/|])

thRelFile :: PLATFORM_PATH
thRelFile = checkInstantiated $(mkRelFile [OsString.pstr|name|])

liftRelDir :: PLATFORM_PATH
liftRelDir = checkInstantiated $(TH.lift (Path [OsString.pstr|name/|] :: Path Rel Dir))

liftRelFile :: PLATFORM_PATH
liftRelFile = checkInstantiated $(TH.lift (Path [OsString.pstr|name|] :: Path Rel File))

liftComplex :: PLATFORM_PATH
liftComplex = toOsPath $(TH.lift (Path [OsString.pstr|name|] :: Path [[Bool]] (Proxy 'True)))
