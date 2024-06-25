{-# LANGUAGE AllowAmbiguousTypes #-}

module OsPath.Aeson.Internal where

import System.IO

newtype AsBinary path = AsBinary { asBinary :: path }

newtype AsText path encoding = AsText { asText :: path }

class IsTextEncoding a where
  textEncoding :: TextEncoding

data Utf8

instance IsTextEncoding Utf8 where
  textEncoding = utf8

data Utf16LE

instance IsTextEncoding Utf16LE where
  textEncoding = utf16le
