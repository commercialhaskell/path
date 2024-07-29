-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_STRING = PosixString | WindowsString
--     PLATFORM_CHAR = PosixChar | WindowsChar
--     IS_WINDOWS = 0 | 1

{-# OPTIONS_GHC -Wno-deprecations #-}

module System.OsString.Compat.PLATFORM_NAME
#if MIN_VERSION_os_string(2,0,0)
  ( OsString.all
  , OsString.any
  , OsString.break
  , OsString.breakEnd
  , OsString.dropWhileEnd
  , OsString.empty
  , OsString.init
  , OsString.isInfixOf
  , OsString.isPrefixOf
  , OsString.isSuffixOf
  , OsString.length
  , OsString.map
  , OsString.null
  , OsString.replicate
  , OsString.singleton
  , OsString.span
  , OsString.spanEnd
  , OsString.stripPrefix
  , OsString.uncons
  )
#else
  ( System.OsString.Compat.PLATFORM_NAME.all
  , System.OsString.Compat.PLATFORM_NAME.any
  , System.OsString.Compat.PLATFORM_NAME.break
  , System.OsString.Compat.PLATFORM_NAME.breakEnd
  , System.OsString.Compat.PLATFORM_NAME.dropWhileEnd
  , System.OsString.Compat.PLATFORM_NAME.empty
  , System.OsString.Compat.PLATFORM_NAME.init
  , System.OsString.Compat.PLATFORM_NAME.isInfixOf
  , System.OsString.Compat.PLATFORM_NAME.isPrefixOf
  , System.OsString.Compat.PLATFORM_NAME.isSuffixOf
  , System.OsString.Compat.PLATFORM_NAME.length
  , System.OsString.Compat.PLATFORM_NAME.map
  , System.OsString.Compat.PLATFORM_NAME.null
  , System.OsString.Compat.PLATFORM_NAME.replicate
  , System.OsString.Compat.PLATFORM_NAME.singleton
  , System.OsString.Compat.PLATFORM_NAME.span
  , System.OsString.Compat.PLATFORM_NAME.spanEnd
  , System.OsString.Compat.PLATFORM_NAME.stripPrefix
  , System.OsString.Compat.PLATFORM_NAME.uncons
  )
#endif
  where

import System.OsString.PLATFORM_NAME as OsString

#if !MIN_VERSION_os_string(2,0,0)
import Data.Coerce (coerce)
import System.OsString.Internal.Types (PLATFORM_STRING(..), PLATFORM_CHAR(..))

#if IS_WINDOWS
import qualified System.OsPath.Data.ByteString.Short.Word16 as BSP
#else
import qualified System.OsPath.Data.ByteString.Short as BSP
#endif
#endif

#if !MIN_VERSION_os_string(2,0,0)
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
