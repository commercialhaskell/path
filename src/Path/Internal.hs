{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Internal types and functions.

module Path.Internal
  ( Path(..)
  , PathRelativity(..)
  , FileType(..)
  , PathParseException(..)
  , parseAbsDir
  , parseAbsFile
  , parseRelDir
  , parseRelFile
  , normalizeDir
  , normalizeFilePath
  , hasParentDir
  )
  where

import           Control.Exception (Exception)
import Control.DeepSeq (NFData (..))
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as Aeson
import Data.Data
import Data.Hashable
import Data.List
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

data PathRelativity
  -- | An absolute path.
  = Abs

  -- | A relative path; one without a root. Note that a @.@ as well as
  -- any path starting with a @..@ is not a valid relative path. In
  -- other words, a relative path is always strictly under the
  -- directory tree to which it is relative.
  | Rel
  deriving (Typeable)

data FileType
  = File
  | Dir
  deriving (Typeable)

-- | Path of some base and type.
--
-- The type variables are:
--
--   * @b@ — base, the base location of the path; absolute or relative.
--   * @t@ — type, whether file or directory.
--
-- Internally is a string. The string can be of two formats only:
--
-- 1. File format: @file.txt@, @foo\/bar.txt@, @\/foo\/bar.txt@
-- 2. Directory format: @foo\/@, @\/foo\/bar\/@
--
-- All directories end in a trailing separator. There are no duplicate
-- path separators @\/\/@, no @..@, no @.\/@, no @~\/@, etc.
newtype Path (b :: PathRelativity) (t :: FileType) = Path FilePath
  deriving (Typeable)

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  | Couldn'tStripPrefixDir FilePath FilePath
  deriving (Show,Typeable)
instance Exception PathParseException

-- | String equality.
--
-- The following property holds:
--
-- @show x == show y ≡ x == y@
instance Eq (Path b t) where
  (==) (Path x) (Path y) = x == y

-- | String ordering.
--
-- The following property holds:
--
-- @show x \`compare\` show y ≡ x \`compare\` y@
instance Ord (Path b t) where
  compare (Path x) (Path y) = compare x y

-- | Same as 'show . Path.toFilePath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b t) where
  show (Path x) = show x

instance NFData (Path b t) where
  rnf (Path x) = rnf x

instance ToJSON (Path b t) where
  toJSON (Path x) = toJSON x
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding (Path x) = toEncoding x
  {-# INLINE toEncoding #-}
#endif

instance Hashable (Path b t) where
  hashWithSalt n (Path path) = hashWithSalt n path

--------------------------------------------------------------------------------
-- Parsers

-- | Convert an absolute 'FilePath' to a normalized absolute dir 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not an absolute path
-- * contains a @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsDir :: MonadThrow m
            => FilePath -> m (Path 'Abs 'Dir)
parseAbsDir filepath =
  if FilePath.isAbsolute filepath &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidAbsDir filepath)

-- | Convert a relative 'FilePath' to a normalized relative dir 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not a relative path
-- * is any of @""@, @.@ or @..@
-- * contains @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path 'Rel 'Dir)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (hasParentDir filepath) &&
     not (null filepath) &&
     filepath /= "." &&
     normalizeFilePath filepath /= curDirNormalizedFP &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidRelDir filepath)

-- | Convert an absolute 'FilePath' to a normalized absolute file 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not an absolute path
-- * has a trailing path separator
-- * contains @..@ anywhere in the path
-- * ends in @/.@
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsFile :: MonadThrow m
             => FilePath -> m (Path 'Abs 'File)
parseAbsFile filepath =
  case validAbsFile filepath of
    True
      | normalized <- normalizeFilePath filepath
      , validAbsFile normalized ->
        return (Path normalized)
    _ -> throwM (InvalidAbsFile filepath)

-- | Is the string a valid absolute file?
validAbsFile :: FilePath -> Bool
validAbsFile filepath =
  FilePath.isAbsolute filepath &&
  not (FilePath.hasTrailingPathSeparator filepath) &&
  not (hasParentDir filepath) &&
  FilePath.isValid filepath

-- | Convert a relative 'FilePath' to a normalized relative file 'Path'.
--
-- Throws: 'PathParseException' when the supplied path:
--
-- * is not a relative path
-- * has a trailing path separator
-- * is @""@, @.@ or @..@
-- * contains @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelFile :: MonadThrow m
             => FilePath -> m (Path 'Rel 'File)
parseRelFile filepath =
  case validRelFile filepath of
    True
      | normalized <- normalizeFilePath filepath
      , validRelFile normalized -> return (Path normalized)
    _ -> throwM (InvalidRelFile filepath)

-- | Is the string a valid relative file?
validRelFile :: FilePath -> Bool
validRelFile filepath =
  not
    (FilePath.isAbsolute filepath || FilePath.hasTrailingPathSeparator filepath) &&
  not (null filepath) &&
  not (hasParentDir filepath) &&
  filepath /= "." && FilePath.isValid filepath

instance FromJSON (Path 'Abs 'File) where
  parseJSON = parseJSONWith parseAbsFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path 'Rel 'File) where
  parseJSON = parseJSONWith parseRelFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path 'Abs 'Dir) where
  parseJSON = parseJSONWith parseAbsDir
  {-# INLINE parseJSON #-}

instance FromJSON (Path 'Rel 'Dir) where
  parseJSON = parseJSONWith parseRelDir
  {-# INLINE parseJSON #-}

parseJSONWith :: (Show e, FromJSON a)
              => (a -> Either e b) -> Aeson.Value -> Aeson.Parser b
parseJSONWith f x =
  do fp <- parseJSON x
     case f fp of
       Right p -> return p
       Left e -> fail (show e)
{-# INLINE parseJSONWith #-}

-- | Helper function: check if the filepath has any parent directories in it.
-- This handles the logic of checking for different path separators on Windows.
hasParentDir :: FilePath -> Bool
hasParentDir filepath' =
     (filepath' == "..") ||
     ("/.." `isSuffixOf` filepath) ||
     ("/../" `isInfixOf` filepath) ||
     ("../" `isPrefixOf` filepath)
  where
    filepath =
        case FilePath.pathSeparator of
            '/' -> filepath'
            x   -> map (\y -> if x == y then '/' else y) filepath'

--------------------------------------------------------------------------------
-- Internal functions

curDirNormalizedFP :: FilePath
curDirNormalizedFP = '.' : [FilePath.pathSeparator]

-- | Internal use for normalizing a directory.
normalizeDir :: FilePath -> FilePath
normalizeDir = FilePath.addTrailingPathSeparator . normalizeFilePath

normalizeFilePath :: FilePath -> FilePath
#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || MIN_VERSION_filepath(1,4,0)
normalizeFilePath = FilePath.normalise
#else
normalizeFilePath = normalizeLeadingSeparators . FilePath.normalise
    where
        sep = FilePath.pathSeparator
        normalizeLeadingSeparators (x1:x2:xs) | x1 == sep && x2 == sep
            = normalizeLeadingSeparators (sep:xs)
        normalizeLeadingSeparators x = x
#endif
