-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_PATH_SINGLE = 'PosixPath' | 'WindowsPath'
--     PLATFORM_CHAR = PosixChar | WindowsChar
--     PLATFORM_CHAR_SINGLE = 'PosixChar' | 'WindowsChar'
--     PLATFORM_WORD = Word8 | Word16
--     PLATFORM_WORD_SINGLE = 'Word8' | 'Word16'
--     PLATFORM_UTF_CODEC = UTF8 | UTF16-LE
--     IS_WINDOWS = 0 | 1

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides the necessary tools to decode\/encode a 'Path'
-- from\/to a 'Value'.
--
-- __Background:__
-- In earlier versions of this library there was only the @Path.Path@ type. The
-- underlying representation of this type is a 'FilePath', and for that type we
-- know the encoding: It is a simple sequence of Unicode codepoints. So there is
-- are \"obivous\" 'FromJSON' and 'ToJSON' instances: We convert from/to a JSON
-- 'Data.Aeson.String'.
--
-- The @OsPath.Path@ type however uses the types found in @System.OsPath@ of the
-- @filepath@ package. These filepaths are represented as a bunch of bytes with
-- no encoding information attached. That means that if a 'Path' is for example
-- passed to 'toJSON' as an argument it is not clear which representation JSON
-- we can choose for JSON. If the path was Unicode-encoded we could convert it
-- to a JSON 'Aeson.String' as before, but we cannot assume that anymore.
-- Hence there are no \"obvious\" 'FromJSON' and 'ToJSON' for 'Path'.
--
-- __What this module provides:__
-- This module defines functions and types suitable to convert a 'Path' from\/to
-- two JSON representations:
--
-- * The /binary/ representation encodes\/decodes a 'Path' as a sequence of
--   numbers in JSON, where each number represents the numeric encoding of one
--   PLATFORM_CHAR_SINGLE of the underlying PLATFORM_PATH_SINGLE:
--
--     >>> Data.Aeson.encode (relFileAsBinary [relfile|foo/bar|])
--     "[102,111,111,92,98,97,114]"
--
--     Note that this is a total encoding since every PLATFORM_PATH_SINGLE can
--     be represented as a bytestring and vice versa.
--
-- * The /textual/ representation tries to encode\/decode a 'Path' as a string
--   in JSON. In order to do that we also have to provide an encoding.
--
--     Some functions in this module take a 'System.IO.TextEncoding' as an
--     argument and you use those defined in "System.IO" or
--     "System.OsString.Encoding":
--
--     >>> Data.Aeson.encode (relFileAsTextWith unicode [relfile|foo/bar|])
--     "\"foo/bar\""
--
--     Other functions expect that the encoding is passed on the type-level
--     (you need the @TypeApplications@ language extensions for this to work):
--
--     >>> Data.Aeson.encode (relFileAsText @Unicode [relfile|foo/bar|])
--     "\"foo/bar\""
--
--     This module provides the encoding types 'Utf8', 'Utf16LE' and 'Unicode',
--     where the latter one of the former two dependenting on the platform.
--
--     __WARNING:__ Decoding and encoding may fail with a
--     'System.OsPath.EncodingException'!
--     The examples above work because 'relfile' encodes to the proper Unicode
--     encoding for the particular platform.
module OsPath.Aeson.PLATFORM_NAME
  ( -- * Conversion functions

    -- ** Binary representation
    -- $binary-rep

    -- *** From JSON
    absDirFromBinary
  , absFileFromBinary
  , relDirFromBinary
  , relFileFromBinary
  , someDirFromBinary
  , someFileFromBinary

    -- *** To JSON
  , pathToBinary
  , absDirToBinary
  , absFileToBinary
  , relDirToBinary
  , relFileToBinary
  , someBaseToBinary
  , someDirToBinary
  , someFileToBinary

    -- ** Textual representation
    -- $textual-rep

    -- *** From JSON
  , pathFromText
  , pathFromTextWith
  , absDirFromText
  , absFileFromText
  , relDirFromText
  , relFileFromText
  , someBaseFromText
  , someBaseFromTextWith
  , someDirFromText
  , someFileFromText

    -- *** To JSON
  , pathToText
  , pathToTextWith
  , absDirToText
  , absFileToText
  , relDirToText
  , relFileToText
  , someBaseToText
  , someBaseToTextWith
  , someDirToText
  , someFileToText

    -- * Conversion using newtype wrappers
    -- $newtype-wrappers
  , AsBinary(..)
  , AsText(..)

    -- * Text encodings
  , IsTextEncoding
  , Unicode
  , Utf8
  , Utf16LE
  , unicode
  ) where

import Control.Exception (displayException)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (Encoding, Parser, Value)
import qualified Data.Aeson.Types as Aeson
import Data.Coerce (coerce)
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (PLATFORM_WORD)
import System.IO
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import System.OsString.Internal.Types (PLATFORM_CHAR(..))

import OsPath.PLATFORM_NAME
import OsPath.Aeson.Internal
import OsPath.Internal.PLATFORM_NAME

--------------------------------------------------------------------------------
-- Conversion functions
--------------------------------------------------------------------------------

unicode :: TextEncoding
#if IS_WINDOWS
unicode = utf16le
#else
unicode = utf8
#endif

   ----------------------------------------
   -- Functions for PLATFORM_PATH
   ----------------------------------------

platformPathFromBinary :: Value -> Parser PLATFORM_PATH
platformPathFromBinary value =
  OsPath.pack . coerce @[PLATFORM_WORD] @[PLATFORM_CHAR] <$> parseJSON value
{-# INLINE platformPathFromBinary #-}

platformPathFromText :: forall enc . IsTextEncoding enc
                     => Value
                     -> Parser PLATFORM_PATH
platformPathFromText = platformPathFromTextWith (textEncoding @enc)
{-# INLINE platformPathFromText #-}

platformPathFromTextWith :: TextEncoding
                         -> Value
                         -> Parser PLATFORM_PATH
platformPathFromTextWith enc = unsafeEncodeWith enc <=< parseJSON
{-# INLINE platformPathFromTextWith #-}

platformPathToBinary :: PLATFORM_PATH -> Value
platformPathToBinary =
    toJSON
  . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
  . OsPath.unpack
{-# INLINE platformPathToBinary #-}

platformPathToBinaryEncoding :: PLATFORM_PATH -> Encoding
platformPathToBinaryEncoding =
    toEncoding
  . coerce @[PLATFORM_CHAR] @[PLATFORM_WORD]
  . OsPath.unpack
{-# INLINE platformPathToBinaryEncoding #-}

platformPathToText :: forall enc . IsTextEncoding enc
                   => PLATFORM_PATH
                   -> Value
platformPathToText = platformPathToTextWith (textEncoding @enc)
{-# INLINE platformPathToText #-}

platformPathToTextWith :: TextEncoding -> PLATFORM_PATH -> Value
platformPathToTextWith enc = toJSON . unsafeDecodeWith enc
{-# INLINE platformPathToTextWith #-}

platformPathToTextEncoding :: forall enc . IsTextEncoding enc
                           => PLATFORM_PATH
                           -> Encoding
platformPathToTextEncoding = platformPathToTextEncodingWith (textEncoding @enc)
{-# INLINE platformPathToTextEncoding #-}

platformPathToTextEncodingWith :: TextEncoding -> PLATFORM_PATH -> Encoding
platformPathToTextEncodingWith enc = toEncoding . unsafeDecodeWith enc
{-# INLINE platformPathToTextEncodingWith #-}

   ----------------------------------------
   -- Functions for Path
   ----------------------------------------

absDirFromBinary :: Value -> Parser (Path Abs Dir)
absDirFromBinary = parseAsBinary parseAbsDir
{-# INLINE absDirFromBinary #-}

absFileFromBinary :: Value -> Parser (Path Abs File)
absFileFromBinary = parseAsBinary parseAbsFile
{-# INLINE absFileFromBinary #-}

relDirFromBinary :: Value -> Parser (Path Rel Dir)
relDirFromBinary = parseAsBinary parseRelDir
{-# INLINE relDirFromBinary #-}

relFileFromBinary :: Value -> Parser (Path Rel File)
relFileFromBinary = parseAsBinary parseRelFile
{-# INLINE relFileFromBinary #-}

pathFromText :: forall enc b t . IsTextEncoding enc
             => (forall m . MonadThrow m => PLATFORM_PATH -> m (Path b t))
             -> Value
             -> Parser (Path b t)
pathFromText = pathFromTextWith  (textEncoding @enc)
{-# INLINE pathFromText #-}

pathFromTextWith :: TextEncoding
                 -> (forall m . MonadThrow m => PLATFORM_PATH -> m (Path b t))
                 -> Value
                 -> Parser (Path b t)
pathFromTextWith = parseTextWith
{-# INLINE pathFromTextWith #-}

absDirFromText :: forall enc . IsTextEncoding enc
               => Value
               -> Parser (Path Abs Dir)
absDirFromText = pathFromText @enc parseAbsDir
{-# INLINE absDirFromText #-}

absFileFromText :: forall enc . IsTextEncoding enc
                => Value
                -> Parser (Path Abs File)
absFileFromText = pathFromText @enc parseAbsFile
{-# INLINE absFileFromText #-}

relDirFromText :: forall enc . IsTextEncoding enc
               => Value
               -> Parser (Path Rel Dir)
relDirFromText = pathFromText @enc parseRelDir
{-# INLINE relDirFromText #-}

relFileFromText :: forall enc . IsTextEncoding enc
                => Value
                -> Parser (Path Rel File)
relFileFromText = pathFromText @enc parseRelFile
{-# INLINE relFileFromText #-}

pathToBinary :: Path b t -> Value
pathToBinary = platformPathToBinary . toOsPath
{-# INLINE pathToBinary #-}

absDirToBinary :: Path Abs Dir -> Value
absDirToBinary = pathToBinary
{-# INLINE absDirToBinary #-}

absFileToBinary :: Path Abs File -> Value
absFileToBinary = pathToBinary
{-# INLINE absFileToBinary #-}

relDirToBinary :: Path Rel Dir -> Value
relDirToBinary = pathToBinary
{-# INLINE relDirToBinary #-}

relFileToBinary :: Path Rel File -> Value
relFileToBinary = pathToBinary
{-# INLINE relFileToBinary #-}

pathToText :: forall enc b t . IsTextEncoding enc => Path b t -> Value
pathToText = platformPathToText @enc . toOsPath
{-# INLINE pathToText #-}

pathToTextWith :: TextEncoding -> Path b t -> Value
pathToTextWith enc = platformPathToTextWith enc . toOsPath
{-# INLINE pathToTextWith #-}

absDirToText :: forall enc . IsTextEncoding enc => Path Abs Dir -> Value
absDirToText = pathToText @enc
{-# INLINE absDirToText #-}

absFileToText :: forall enc . IsTextEncoding enc => Path Abs File -> Value
absFileToText = pathToText @enc
{-# INLINE absFileToText #-}

relDirToText :: forall enc . IsTextEncoding enc => Path Rel Dir -> Value
relDirToText = pathToText @enc
{-# INLINE relDirToText #-}

relFileToText :: forall enc . IsTextEncoding enc => Path Rel File -> Value
relFileToText = pathToText @enc
{-# INLINE relFileToText #-}

   ----------------------------------------
   -- Functions for SomeBase
   ----------------------------------------

someDirFromBinary :: Value -> Parser (SomeBase Dir)
someDirFromBinary = parseAsBinary parseSomeDir
{-# INLINE someDirFromBinary #-}

someFileFromBinary :: Value -> Parser (SomeBase File)
someFileFromBinary = parseAsBinary parseSomeFile
{-# INLINE someFileFromBinary #-}

someBaseFromText :: forall enc t . IsTextEncoding enc
                 => (forall m . MonadThrow m => PLATFORM_PATH -> m (SomeBase t))
                 -> Value
                 -> Parser (SomeBase t)
someBaseFromText = someBaseFromTextWith (textEncoding @enc)
{-# INLINE someBaseFromText #-}

someBaseFromTextWith :: TextEncoding
                     -> (forall m . MonadThrow m => PLATFORM_PATH -> m (SomeBase t))
                     -> Value
                     -> Parser (SomeBase t)
someBaseFromTextWith = parseTextWith
{-# INLINE someBaseFromTextWith #-}

someDirFromText :: forall enc . IsTextEncoding enc
                => Value
                -> Parser (SomeBase Dir)
someDirFromText = someBaseFromText @enc parseSomeDir
{-# INLINE someDirFromText #-}

someFileFromText :: forall enc . IsTextEncoding enc
                 => Value
                 -> Parser (SomeBase File)
someFileFromText = someBaseFromText @enc parseSomeFile
{-# INLINE someFileFromText #-}

someBaseToBinary :: SomeBase t -> Value
someBaseToBinary = platformPathToBinary . fromSomeBase
{-# INLINE someBaseToBinary #-}

someDirToBinary :: SomeBase Dir -> Value
someDirToBinary = someBaseToBinary
{-# INLINE someDirToBinary #-}

someFileToBinary :: SomeBase File -> Value
someFileToBinary = someBaseToBinary
{-# INLINE someFileToBinary #-}

someBaseToText :: forall enc t . IsTextEncoding enc => SomeBase t -> Value
someBaseToText = platformPathToText @enc . fromSomeBase
{-# INLINE someBaseToText #-}

someBaseToTextWith :: TextEncoding -> SomeBase t -> Value
someBaseToTextWith enc = platformPathToTextWith enc . fromSomeBase
{-# INLINE someBaseToTextWith #-}

someDirToText :: forall enc . IsTextEncoding enc => SomeBase Dir -> Value
someDirToText = someBaseToText @enc
{-# INLINE someDirToText #-}

someFileToText :: forall enc . IsTextEncoding enc => SomeBase Dir -> Value
someFileToText = someBaseToText @enc
{-# INLINE someFileToText #-}

--------------------------------------------------------------------------------
-- Default instances
--------------------------------------------------------------------------------

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode (Path b t)) instance FromJSON (AsText Unicode (Path b t)) => FromJSON (Path b t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode (Path b t)) instance ToJSON (AsText Unicode (Path b t)) => ToJSON (Path b t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode (Path b t)) instance FromJSONKey (AsText Unicode (Path b t)) => FromJSONKey (Path b t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode (Path b t)) instance ToJSONKey (AsText Unicode (Path b t)) => ToJSONKey (Path b t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode (SomeBase t)) instance FromJSON (AsText Unicode (SomeBase t)) => FromJSON (SomeBase t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode (SomeBase t)) instance ToJSON (SomeBase t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If decoding fails a runtime error will be thrown.
deriving via (AsText Unicode (SomeBase t)) instance FromJSONKey (AsText Unicode (SomeBase t)) => FromJSONKey (SomeBase t)

-- | This instance assumes that the underlying PLATFORM_PATH_SINGLE is
-- PLATFORM_UTF_CODEC encoded. If encoding fails a runtime error will be thrown.
deriving via (AsText Unicode (SomeBase t)) instance ToJSONKey (SomeBase t)

--------------------------------------------------------------------------------
-- Instances for newtype wrappers
--------------------------------------------------------------------------------

-- $newtype-wrappers
-- This module defines two newtype wrappers to control the conversion between
-- JSON value and a 'Path':
--
--  * 'AsBinary' represents a 'Path' as a sequence of PLATFORM_WORD_SINGLE in
--    JSON. For example:
--
--    >>> Data.Aeson.encode (AsBinary [relfile|foo/bar|])
--    "[102,111,111,92,98,97,114]"
--
--    Note that this is a total encoding since every PLATFORM_PATH_SINGLE can be
--    represented as a byte array and vice versa.
--
--  * 'AsText' tries to represent a 'Path' as a string in JSON. In order to do
--    that we also have to provide an encoding. Those are represented as by
--    'Utf8', 'Utf16LE' and 'Unicode', where the latter is a type synonym for
--    'Utf8' for POSIX paths and 'Utf16LE' for Windows paths.
--    Note that this may fail with a runtime error if the underlying
--    PLATFORM_PATH_SINGLE uses a different encoding!
--    Since 'relfile' uses a unicode encoding the previous example displays as
--    follows (you need the @TypeApplications@ language extensions for this to
--    work):
--
--    >>> Data.Aeson.encode (AsText @Unicode [relfile|foo/bar|])
--    "foo/bar"

#if IS_WINDOWS
type Unicode = Utf16LE
#else
type Unicode = Utf8
#endif

   ----------------------------------------
   -- Instances for PLATFORM_PATH
   ----------------------------------------

instance FromJSON (AsBinary PLATFORM_PATH) where
  parseJSON value = AsBinary <$> platformPathFromBinary value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc PLATFORM_PATH) where
  parseJSON value = AsText <$> platformPathFromText @enc value
  {-# INLINE parseJSON #-}

instance ToJSON (AsBinary PLATFORM_PATH) where
  toJSON = platformPathToBinary . asBinary
  {-# INLINE toJSON #-}
  toEncoding = platformPathToBinaryEncoding . asBinary
  {-# INLINE toEncoding #-}

instance IsTextEncoding enc => ToJSON (AsText enc PLATFORM_PATH) where
  toJSON = platformPathToText @enc . asText
  {-# INLINE toJSON #-}
  toEncoding = platformPathToTextEncoding @enc . asText
  {-# INLINE toEncoding #-}

instance FromJSONKey (AsBinary PLATFORM_PATH) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc PLATFORM_PATH) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText pure)
  {-# INLINE fromJSONKey #-}

instance ToJSONKey (AsBinary PLATFORM_PATH) where
  toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
  {-# INLINE toJSONKey #-}

instance IsTextEncoding enc => ToJSONKey (AsText enc PLATFORM_PATH) where
  toJSONKey = Aeson.toJSONKeyText decodeAsText
  {-# INLINE toJSONKey #-}

   ----------------------------------------
   -- Instances for Path
   ----------------------------------------

instance FromJSON (AsBinary (Path Abs Dir)) where
  parseJSON value = AsBinary <$> absDirFromBinary value
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Abs File)) where
  parseJSON value = AsBinary <$> absFileFromBinary value
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Rel Dir)) where
  parseJSON value = AsBinary <$> relDirFromBinary value
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (Path Rel File)) where
  parseJSON value = AsBinary <$> relFileFromBinary value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (Path Abs Dir)) where
  parseJSON value = AsText <$> absDirFromText @enc value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (Path Abs File)) where
  parseJSON value = AsText <$> absFileFromText @enc value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (Path Rel Dir)) where
  parseJSON value = AsText <$> relDirFromText @enc value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (Path Rel File)) where
  parseJSON value = AsText <$> relFileFromText @enc value
  {-# INLINE parseJSON #-}

deriving via (AsBinary PLATFORM_PATH) instance ToJSON (AsBinary (Path b t))
deriving via (AsText enc PLATFORM_PATH) instance IsTextEncoding enc => ToJSON (AsText enc (Path b t))

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

instance IsTextEncoding enc => FromJSONKey (AsText enc (Path Abs Dir)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseAbsDir)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc (Path Abs File)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseAbsFile)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc (Path Rel Dir)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseRelDir)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc (Path Rel File)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseRelFile)
  {-# INLINE fromJSONKey #-}

deriving via (AsBinary PLATFORM_PATH) instance ToJSONKey (AsBinary (Path b t))
deriving via (AsText enc PLATFORM_PATH) instance IsTextEncoding enc => ToJSONKey (AsText enc (Path b t))

   ----------------------------------------
   -- Instances for SomeBase
   ----------------------------------------

instance FromJSON (AsBinary (SomeBase Dir)) where
  parseJSON value = AsBinary <$> someDirFromBinary value
  {-# INLINE parseJSON #-}

instance FromJSON (AsBinary (SomeBase File)) where
  parseJSON value = AsBinary <$> someFileFromBinary value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (SomeBase Dir)) where
  parseJSON value = AsText <$> someDirFromText @enc value
  {-# INLINE parseJSON #-}

instance IsTextEncoding enc => FromJSON (AsText enc (SomeBase File)) where
  parseJSON value = AsText <$> someFileFromText @enc value
  {-# INLINE parseJSON #-}

instance ToJSON (AsBinary (SomeBase t)) where
  toJSON = toJSON . fmap fromSomeBase
  {-# INLINE toJSON #-}
  toEncoding = toEncoding . fmap fromSomeBase
  {-# INLINE toEncoding #-}

instance IsTextEncoding enc => ToJSON (AsText enc (SomeBase t)) where
  toJSON = toJSON . fmap fromSomeBase
  {-# INLINE toJSON #-}
  toEncoding = toEncoding . fmap fromSomeBase
  {-# INLINE toEncoding #-}

instance FromJSONKey (AsBinary (SomeBase Dir)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance FromJSONKey (AsBinary (SomeBase File)) where
  fromJSONKey = Aeson.FromJSONKeyValue parseJSON
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc (SomeBase Dir)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseSomeDir)
  {-# INLINE fromJSONKey #-}

instance IsTextEncoding enc => FromJSONKey (AsText enc (SomeBase File)) where
  fromJSONKey = Aeson.FromJSONKeyTextParser (parseKeyAsText parseSomeFile)
  {-# INLINE fromJSONKey #-}

instance ToJSONKey (AsBinary (SomeBase t)) where
  toJSONKey = Aeson.ToJSONKeyValue toJSON toEncoding
  {-# INLINE toJSONKey #-}

instance IsTextEncoding enc => ToJSONKey (AsText enc (SomeBase t)) where
  toJSONKey = fmap fromSomeBase >$< toJSONKey
  {-# INLINE toJSONKey #-}

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

unsafeDecodeWith :: TextEncoding -> PLATFORM_PATH -> String
unsafeDecodeWith enc =
  either (error . displayException) id . OsPath.decodeWith enc
{-# INLINE unsafeDecodeWith #-}

unsafeEncodeWith :: TextEncoding -> String -> Parser PLATFORM_PATH
unsafeEncodeWith enc =
  either (fail . displayException) pure . OsPath.encodeWith enc
{-# INLINE unsafeEncodeWith #-}

decodeAsText :: forall enc . IsTextEncoding enc
             => AsText enc PLATFORM_PATH
             -> Text
decodeAsText = Text.pack . unsafeDecodeWith (textEncoding @enc) . asText
{-# INLINE decodeAsText #-}

parseAsBinary :: (forall m . MonadThrow m => PLATFORM_PATH -> m a)
              -> Value
              -> Parser a
parseAsBinary parse =
  either (fail . displayException) pure . parse <=< platformPathFromBinary
{-# INLINE parseAsBinary #-}

parseTextWith :: TextEncoding
              -> (forall m . MonadThrow m => PLATFORM_PATH -> m a)
              -> Value
              -> Parser a
parseTextWith enc parse =
  either (fail . displayException) pure . parse <=< platformPathFromTextWith enc
{-# INLINE parseTextWith #-}

parseKeyAsText :: forall enc a . IsTextEncoding enc
               => (forall m . MonadThrow m => PLATFORM_PATH -> m a)
               -> Text
               -> Parser (AsText enc a)
parseKeyAsText parse =
  either (fail . displayException) (pure . AsText) . parse
  <=< unsafeEncodeWith (textEncoding @enc) . Text.unpack
{-# INLINE parseKeyAsText #-}
