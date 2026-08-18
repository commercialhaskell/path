-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_STRING = PosixString | WindowsString
--     PLATFORM_CHAR = PosixChar | WindowsChar
--     IS_WINDOWS = 0 | 1

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#define USE_os_string 0
#if defined MIN_VERSION_os_string
#if MIN_VERSION_os_string(2,0,0)
#undef USE_os_string
#define USE_os_string 1
#endif
#endif

#ifndef MODULE_NAME
#define MODULE_NAME System.OsString.Compat.PLATFORM_NAME
#endif

module MODULE_NAME
#if USE_os_string
  ( PLATFORM_STRING(..)
  , PLATFORM_CHAR(..)
  , module OsString
#ifdef ALTERNATE_OSSTRING_PSTR
  , pstr
#endif
  )
#else
  ( PLATFORM_STRING(..)
  , PLATFORM_CHAR(..)
#ifndef ALTERNATE_OSSTRING_PSTR
  , OsString.pstr
#else
  , pstr
#endif
  , MODULE_NAME.all
  , MODULE_NAME.any
  , MODULE_NAME.break
  , MODULE_NAME.breakEnd
  , MODULE_NAME.dropWhileEnd
  , MODULE_NAME.empty
  , MODULE_NAME.init
  , MODULE_NAME.isInfixOf
  , MODULE_NAME.isPrefixOf
  , MODULE_NAME.isSuffixOf
  , MODULE_NAME.length
  , MODULE_NAME.map
  , MODULE_NAME.null
  , MODULE_NAME.replicate
  , MODULE_NAME.singleton
  , MODULE_NAME.span
  , MODULE_NAME.spanEnd
  , MODULE_NAME.stripPrefix
  , MODULE_NAME.uncons
  )
#endif
  where

import Data.Data (Data)
#ifdef USE_os_string
#ifdef EXTRA_DATA_DERIVATION
import System.OsString.Internal.Types (PLATFORM_STRING(..), PLATFORM_CHAR(..), EXTRA_DATA_DERIVATION)
#else
import System.OsString.Internal.Types (PLATFORM_STRING(..), PLATFORM_CHAR(..))
#endif
#else
import System.OsString (PLATFORM_STRING(..), PLATFORM_CHAR(..))
#ifdef EXTRA_DATA_DERIVATION
import System.OsString.Types (EXTRA_DATA_DERIVATION)
#endif
#endif

#ifndef OSSTRING_MODULE
#define OSSTRING_MODULE System.OsString.PLATFORM_NAME
#endif

import OSSTRING_MODULE as OsString

#if !USE_os_string
import Data.Coerce (coerce)

#if IS_WINDOWS
import qualified System.OsPath.Data.ByteString.Short.Word16 as BSP
#else
import qualified System.OsPath.Data.ByteString.Short as BSP
#endif
#endif

#ifdef EXPOSE_CONSTRUCTOR_MODULE
import qualified EXPOSE_CONSTRUCTOR_MODULE
#endif

#ifdef ALTERNATE_OSSTRING_PSTR
import Language.Haskell.TH.Quote (QuasiQuoter)
#endif

#ifdef EXTRA_DATA_DERIVATION
deriving instance Data EXTRA_DATA_DERIVATION
#endif

deriving instance Data PLATFORM_STRING

#ifdef ALTERNATE_OSSTRING_PSTR
pstr :: QuasiQuoter
pstr = ALTERNATE_OSSTRING_PSTR
#endif

#if !USE_os_string

all :: (PLATFORM_CHAR -> Bool) -> PLATFORM_STRING -> Bool
all = coerce BSP.all

any :: (PLATFORM_CHAR -> Bool) -> PLATFORM_STRING -> Bool
any = coerce BSP.any

break
  :: (PLATFORM_CHAR -> Bool)
  -> PLATFORM_STRING
  -> (PLATFORM_STRING, PLATFORM_STRING)
break = coerce BSP.break

breakEnd
  :: (PLATFORM_CHAR -> Bool)
  -> PLATFORM_STRING
  -> (PLATFORM_STRING, PLATFORM_STRING)
breakEnd = coerce BSP.breakEnd

dropWhileEnd :: (PLATFORM_CHAR -> Bool) -> PLATFORM_STRING -> PLATFORM_STRING
dropWhileEnd = coerce BSP.dropWhileEnd

empty :: PLATFORM_STRING
empty = coerce BSP.empty

init :: PLATFORM_STRING -> PLATFORM_STRING
init = coerce BSP.init

isInfixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isInfixOf = coerce BSP.isInfixOf

isPrefixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isPrefixOf = coerce BSP.isPrefixOf

isSuffixOf :: PLATFORM_STRING -> PLATFORM_STRING -> Bool
isSuffixOf = coerce BSP.isSuffixOf

length :: PLATFORM_STRING -> Int
length = coerce BSP.length

map :: (PLATFORM_CHAR -> PLATFORM_CHAR) -> PLATFORM_STRING -> PLATFORM_STRING
map = coerce BSP.map

null :: PLATFORM_STRING -> Bool
null = coerce BSP.null

replicate :: Int -> PLATFORM_CHAR -> PLATFORM_STRING
replicate = coerce BSP.replicate

singleton :: PLATFORM_CHAR -> PLATFORM_STRING
singleton = coerce BSP.singleton

span
  :: (PLATFORM_CHAR -> Bool)
  -> PLATFORM_STRING
  -> (PLATFORM_STRING, PLATFORM_STRING)
span = coerce BSP.span

spanEnd
  :: (PLATFORM_CHAR -> Bool)
  -> PLATFORM_STRING
  -> (PLATFORM_STRING, PLATFORM_STRING)
spanEnd = coerce BSP.spanEnd

stripPrefix :: PLATFORM_STRING -> PLATFORM_STRING -> Maybe PLATFORM_STRING
stripPrefix = coerce BSP.stripPrefix

uncons :: PLATFORM_STRING -> Maybe (PLATFORM_CHAR, PLATFORM_STRING)
uncons = coerce BSP.uncons
#endif
