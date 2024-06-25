-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_PATH_SINGLE = 'PosixPath' | 'WindowsPath'
--     PLATFORM_UTF_CODEC = UTF8 | UTF16-LE

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OsPath.Aeson.PLATFORM_NAME () where

import Control.Exception (displayException)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath

import OsPath.PLATFORM_NAME

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSON (Path Abs File) where
  parseJSON = parseJSONWith parseAbsFile
  {-# INLINE parseJSON #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSON (Path Rel File) where
  parseJSON = parseJSONWith parseRelFile
  {-# INLINE parseJSON #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSON (Path Abs Dir) where
  parseJSON = parseJSONWith parseAbsDir
  {-# INLINE parseJSON #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSON (Path Rel Dir) where
  parseJSON = parseJSONWith parseRelDir
  {-# INLINE parseJSON #-}

parseJSONWith :: (forall m . MonadThrow m => PLATFORM_PATH -> m a)
              -> Aeson.Value
              -> Aeson.Parser a
parseJSONWith f x =
  do fp <- parseJSON x
     either (fail . displayException) return $ do
       ospath <- OsPath.encodeUtf fp
       f ospath
{-# INLINE parseJSONWith #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSONKey (Path Abs File) where
  fromJSONKey = fromJSONKeyWith parseAbsFile
  {-# INLINE fromJSONKey #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSONKey (Path Rel File) where
  fromJSONKey = fromJSONKeyWith parseRelFile
  {-# INLINE fromJSONKey #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSONKey (Path Abs Dir) where
  fromJSONKey = fromJSONKeyWith parseAbsDir
  {-# INLINE fromJSONKey #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSONKey (Path Rel Dir) where
  fromJSONKey = fromJSONKeyWith parseRelDir
  {-# INLINE fromJSONKey #-}

fromJSONKeyWith :: (forall m . MonadThrow m => PLATFORM_PATH -> m b)
                -> Aeson.FromJSONKeyFunction b
fromJSONKeyWith f =
  Aeson.FromJSONKeyTextParser $ \text ->
    either (fail . displayException) return $ do
      ospath <- (OsPath.encodeUtf . Text.unpack) text
      f ospath
{-# INLINE fromJSONKeyWith #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
instance ToJSON (Path b t) where
  toJSON =
      either (error . displayException) toJSON
    . OsPath.decodeUtf
    . toOsPath
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding =
      either (error . displayException) toEncoding
    . OsPath.decodeUtf
    . toOsPath
  {-# INLINE toEncoding #-}
#endif

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
instance ToJSONKey (Path b t) where
  toJSONKey = Aeson.toJSONKeyText
    ( either (error . displayException) Text.pack
    . OsPath.decodeUtf
    . toOsPath
    )
  {-# INLINE toJSONKey #-}

instance FromJSON (SomeBase Dir) where
  parseJSON = parseJSONWith parseSomeDir
  {-# INLINE parseJSON #-}

instance FromJSON (SomeBase File) where
  parseJSON = parseJSONWith parseSomeFile
  {-# INLINE parseJSON #-}

instance ToJSON (SomeBase t) where
  toJSON = prjSomeBase toJSON
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = prjSomeBase toEncoding
  {-# INLINE toEncoding #-}
#endif
