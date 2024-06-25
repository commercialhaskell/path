-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_PATH_SINGLE = 'PosixPath' | 'WindowsPath'
--     PLATFORM_CHAR = PosixChar | WindowsChar
--     PLATFORM_WORD = Word8 | Word16
--     PLATFORM_UTF_CODEC = UTF8 | UTF16-LE
--     IS_WINDOWS = 0 | 1

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OsPath.Aeson.PLATFORM_NAME
  ( AsBinary(..)
  , AsText(..)
  , IsTextEncoding
  , Unicode
  , Utf8
  , Utf16LE
  ) where

import Control.Exception (displayException)
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (Parser, Value)
import qualified Data.Aeson.Types as Aeson
import Data.Coerce (coerce)
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (PLATFORM_WORD)
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import System.OsString.Internal.Types (PLATFORM_CHAR(..))

import OsPath.PLATFORM_NAME
import OsPath.Aeson.Internal
import OsPath.Internal.PLATFORM_NAME

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance FromJSON (AsText (Path b t) Unicode) => FromJSON (Path b t) where
  parseJSON = fmap (asText @(Path b t) @Unicode) . parseJSON
  {-# INLINE parseJSON #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
instance ToJSON (Path b t) where
  toJSON = toJSON . AsText @(Path b t) @Unicode
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = toEncoding . AsText @(Path b t) @Unicode
  {-# INLINE toEncoding #-}
#endif

#if MIN_VERSION_aeson(1,0,0)
-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance (FromJSON (AsText (Path b t) Unicode), FromJSONKey (AsText (Path b t) Unicode)) => FromJSONKey (Path b t) where
  fromJSONKey = asText @(Path b t) @Unicode <$> fromJSONKey
  {-# INLINE fromJSONKey #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
instance ToJSONKey (Path b t) where
  toJSONKey = AsText @(Path b t) @Unicode >$< toJSONKey
  {-# INLINE toJSONKey #-}
#endif

instance FromJSON (AsText (SomeBase t) Unicode) => FromJSON (SomeBase t) where
  parseJSON = fmap (asText @(SomeBase t) @Unicode) . parseJSON
  {-# INLINE parseJSON #-}

instance ToJSON (SomeBase t) where
  toJSON = toJSON . AsText @(SomeBase t) @Unicode
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = toEncoding . AsText @(SomeBase t) @Unicode
  {-# INLINE toEncoding #-}
#endif

#if MIN_VERSION_aeson(1,0,0)
-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
instance (FromJSON (AsText (SomeBase t) Unicode), FromJSONKey (AsText (SomeBase t) Unicode)) => FromJSONKey (SomeBase t) where
  fromJSONKey = asText @(SomeBase t) @Unicode <$> fromJSONKey
  {-# INLINE fromJSONKey #-}

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
instance ToJSONKey (SomeBase t) where
  toJSONKey = AsText @(SomeBase t) @Unicode >$< toJSONKey
  {-# INLINE toJSONKey #-}
#endif

--------------------------------------------------------------------------------
-- Instances for newtype wrappers
--------------------------------------------------------------------------------

#if IS_WINDOWS
type Unicode = Utf16LE
#else
type Unicode = Utf8
#endif

   ----------------------------------------
   -- Instances for PLATFORM_PATH
   ----------------------------------------

instance FromJSON (AsBinary PLATFORM_PATH) where
  parseJSON value =
    AsBinary . OsPath.pack . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR]
      <$> parseJSON value
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText PLATFORM_PATH encoding) where
  parseJSON value =
      either (fail . displayException) (pure . AsText)
    . OsPath.encodeWith (textEncoding @encoding)
      =<< parseJSON value
  {-# INLINE parseJSON #-}

instance ToJSON (AsBinary PLATFORM_PATH) where
  toJSON =
      toJSON
    . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
    . OsPath.unpack
    . asBinary
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding =
      toEncoding
    . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
    . OsPath.unpack
    . asBinary
  {-# INLINE toEncoding #-}
#endif

instance IsTextEncoding encoding => ToJSON (AsText PLATFORM_PATH encoding) where
  toJSON =
      either (error . displayException) toJSON
    . OsPath.decodeWith (textEncoding @encoding)
    . asText
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding =
      either (error . displayException) toEncoding
    . OsPath.decodeWith (textEncoding @encoding)
    . asText
  {-# INLINE toEncoding #-}
#endif

#if MIN_VERSION_aeson(1,0,0)
instance FromJSONKey (AsBinary PLATFORM_PATH) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding encoding => FromJSONKey (AsText PLATFORM_PATH encoding) where
  fromJSONKey = Aeson.FromJSONKeyTextParser
    ( either (fail . displayException) (pure . AsText)
    . OsPath.encodeWith (textEncoding @encoding)
    . Text.unpack
    )
  {-# INLINE fromJSONKey #-}

instance ToJSONKey (AsBinary PLATFORM_PATH) where
  toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
  {-# INLINE toJSONKey #-}

instance IsTextEncoding encoding => ToJSONKey (AsText PLATFORM_PATH encoding) where
  toJSONKey = Aeson.toJSONKeyText decodeAsText
  {-# INLINE toJSONKey #-}

decodeAsText :: forall encoding .
  IsTextEncoding encoding => AsText PLATFORM_PATH encoding -> Text
decodeAsText =
    either (error . displayException) Text.pack
  . OsPath.decodeWith (textEncoding @encoding)
  . asText
{-# INLINE decodeAsText #-}
#endif

   ----------------------------------------
   -- Instances for Path
   ----------------------------------------

instance FromJSON (AsBinary (Path Abs Dir)) where
  parseJSON = parseAsBinary parseAbsDir
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Abs File)) where
  parseJSON = parseAsBinary parseAbsFile
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Rel Dir)) where
  parseJSON = parseAsBinary parseRelDir
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Rel File)) where
  parseJSON = parseAsBinary parseRelFile
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (Path Abs Dir) encoding) where
  parseJSON = parseAsText parseAbsDir
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (Path Abs File) encoding) where
  parseJSON = parseAsText parseAbsFile
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (Path Rel Dir) encoding) where
  parseJSON = parseAsText parseRelDir
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (Path Rel File) encoding) where
  parseJSON = parseAsText parseRelFile
  {-# INLINE parseJSON #-}

deriving via (AsBinary PLATFORM_PATH) instance ToJSON (AsBinary (Path b t))
deriving via (AsText PLATFORM_PATH encoding) instance IsTextEncoding encoding => ToJSON (AsText (Path b t) encoding)

#if MIN_VERSION_aeson(1,0,0)
instance FromJSONKey (AsBinary (Path Abs Dir)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (AsBinary (Path Abs File)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (AsBinary (Path Rel Dir)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (AsBinary (Path Rel File)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding encoding => FromJSONKey (AsText (Path Abs Dir) encoding) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseAbsDir)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding encoding => FromJSONKey (AsText (Path Abs File) encoding) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseAbsFile)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding encoding => FromJSONKey (AsText (Path Rel Dir) encoding) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseRelDir)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding encoding => FromJSONKey (AsText (Path Rel File) encoding) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseRelFile)
  {-# INLINE fromJSONKey #-}

deriving via (AsBinary PLATFORM_PATH) instance ToJSONKey (AsBinary (Path b t))
deriving via (AsText PLATFORM_PATH encoding) instance IsTextEncoding encoding => ToJSONKey (AsText (Path b t) encoding)
#endif

   ----------------------------------------
   -- Instances for SomeBase
   ----------------------------------------

instance FromJSON (AsBinary (SomeBase Dir)) where
  parseJSON = parseAsBinary parseSomeDir
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (SomeBase File)) where
  parseJSON = parseAsBinary parseSomeFile
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (SomeBase Dir) encoding) where
  parseJSON = parseAsText parseSomeDir
  {-# INLINE parseJSON #-}

instance IsTextEncoding encoding => FromJSON (AsText (SomeBase File) encoding) where
  parseJSON = parseAsText parseSomeFile
  {-# INLINE parseJSON #-}

instance ToJSON (AsBinary (SomeBase t)) where
  toJSON = prjSomeBase (toJSON . AsBinary) . asBinary
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = prjSomeBase (toEncoding . AsBinary) . asBinary
  {-# INLINE toEncoding #-}
#endif

instance IsTextEncoding encoding => ToJSON (AsText (SomeBase t) encoding) where
  toJSON = prjSomeBase (toJSON . AsText @_ @encoding) . asText
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = prjSomeBase (toEncoding . AsText @_ @encoding) . asText
  {-# INLINE toEncoding #-}
#endif

#if MIN_VERSION_aeson(1,0,0)
instance ToJSONKey (AsBinary (SomeBase t)) where
  toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
  {-# INLINE toJSONKey #-}

instance IsTextEncoding encoding => ToJSONKey (AsText (SomeBase t) encoding) where
  toJSONKey = Aeson.toJSONKeyText
    ( prjSomeBase (decodeAsText . AsText @PLATFORM_PATH @encoding . toOsPath)
    . asText
    )
  {-# INLINE toJSONKey #-}
#endif

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

parseAsBinary :: (forall m . MonadThrow m => PLATFORM_PATH -> m a)
              -> Value
              -> Parser (AsBinary a)
parseAsBinary parse value =
    either (fail . displayException) (pure . AsBinary)
  . parse
  . asBinary
  =<< parseJSON value
{-# INLINE parseAsBinary #-}

parseAsText :: forall encoding a . IsTextEncoding encoding
            => (forall m . MonadThrow m => PLATFORM_PATH -> m a)
            -> Value
            -> Parser (AsText a encoding)
parseAsText parse value =
    either (fail . displayException) (pure . AsText)
  . parse
  . asText @_ @encoding
  =<< parseJSON value
{-# INLINE parseAsText #-}

parseKeyAsText :: forall encoding a . IsTextEncoding encoding
               => (forall m . MonadThrow m => PLATFORM_PATH -> m a)
               -> Text
               -> Parser (AsText a encoding)
parseKeyAsText parse text = do
  ospath <- either (fail . displayException) pure
          . OsPath.encodeWith (textEncoding @encoding)
          . Text.unpack
          $ text
  either (fail . displayException) (pure . AsText) (parse ospath)
{-# INLINE parseKeyAsText #-}
