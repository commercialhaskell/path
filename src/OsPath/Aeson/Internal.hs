{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveTraversable #-}

module OsPath.Aeson.Internal where

import System.IO (TextEncoding, utf8, utf16le)

newtype AsBinary a = AsBinary { asBinary :: a }
  deriving (Foldable, Functor, Traversable)

newtype AsText encoding a = AsText { asText :: a }
  deriving (Foldable, Functor, Traversable)

class IsTextEncoding a where
  textEncoding :: TextEncoding

data Utf8

instance IsTextEncoding Utf8 where
  textEncoding = utf8
  {-# INLINE textEncoding #-}

data Utf16LE

instance IsTextEncoding Utf16LE where
  textEncoding = utf16le
  {-# INLINE textEncoding #-}
