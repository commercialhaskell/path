-- |
-- Module      :  Path
-- Copyright   :  © 2015–2016 FP Complete
-- License     :  BSD 3 clause
--
-- Maintainer  :  Chris Done <chrisdone@fpcomplete.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Support for well-typed paths.

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

module Path
  (-- * Types
   Path
  ,Abs
  ,Rel
  ,File
  ,Dir
   -- * Exceptions
  ,PathException(..)
  ,PathParseException
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
  -- * Constructors
  ,mkAbsDir
  ,mkRelDir
  ,mkAbsFile
  ,mkRelFile
  -- * Operations
  ,(</>)
  ,stripDir
  ,isParentOf
  ,parent
  ,filename
  ,dirname
  -- * Conversion
  ,toFilePath
  ,fromAbsDir
  ,fromRelDir
  ,fromAbsFile
  ,fromRelFile
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Aeson (FromJSON (..))
import qualified Data.Aeson.Types as Aeson
import           Data.Data
import           Data.List
import           Data.Maybe
import           Language.Haskell.TH
import           Path.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs deriving (Typeable)

-- | A relative path; one without a root. Note that a @.@ as well as any path
-- starting with a @..@ is not a valid relative path. In other words, a
-- relative path is always strictly under the directory tree to which it is
-- relative.
data Rel deriving (Typeable)

-- | A file path.
data File deriving (Typeable)

-- | A directory path.
data Dir deriving (Typeable)

instance FromJSON (Path Abs File) where
  parseJSON = parseJSONWith parseAbsFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path Rel File) where
  parseJSON = parseJSONWith parseRelFile
  {-# INLINE parseJSON #-}

instance FromJSON (Path Abs Dir) where
  parseJSON = parseJSONWith parseAbsDir
  {-# INLINE parseJSON #-}

instance FromJSON (Path Rel Dir) where
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

-- | Exceptions during path operations
data PathException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  | InvalidPrefix FilePath FilePath
  | HasNoParent FilePath
  deriving (Show,Typeable)
instance Exception PathException

{-# DEPRECATED PathParseException "Please use PathException instead." #-}
type PathParseException = PathException

--------------------------------------------------------------------------------
-- Parsers

-- | Convert an absolute 'FilePath' to a normalized absolute dir 'Path'.
--
-- Throws: 'InvalidAbsDir' when the supplied path:
--
-- * is not an absolute path
-- * contains a @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsDir :: MonadThrow m
            => FilePath -> m (Path Abs Dir)
parseAbsDir filepath =
  if FilePath.isAbsolute filepath &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidAbsDir filepath)

-- | Convert a relative 'FilePath' to a normalized relative dir 'Path'.
--
-- Throws: 'InvalidRelDir' when the supplied path:
--
-- * is not a relative path
-- * is any of @""@, @.@ or @..@
-- * contains @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path Rel Dir)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (hasParentDir filepath) &&
     not (null filepath) &&
     filepath /= "." && (normalizeFilePath filepath) /= curDirNormalizedFP &&
     filepath /= ".." &&
     FilePath.isValid filepath
     then return (Path (normalizeDir filepath))
     else throwM (InvalidRelDir filepath)

-- | Convert an absolute 'FilePath' to a normalized absolute file 'Path'.
--
-- Throws: 'InvalidAbsFile' when the supplied path:
--
-- * is not an absolute path
-- * has a trailing path separator
-- * contains @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseAbsFile :: MonadThrow m
             => FilePath -> m (Path Abs File)
parseAbsFile filepath =
  if FilePath.isAbsolute filepath &&
     not (FilePath.hasTrailingPathSeparator filepath) &&
     not (hasParentDir filepath) &&
     FilePath.isValid filepath
     then return (Path (normalizeFilePath filepath))
     else throwM (InvalidAbsFile filepath)

-- | Convert a relative 'FilePath' to a normalized relative file 'Path'.
--
-- Throws: 'InvalidRelFile' when the supplied path:
--
-- * is not a relative path
-- * has a trailing path separator
-- * is @""@, @.@ or @..@
-- * contains @..@ anywhere in the path
-- * is not a valid path (See 'System.FilePath.isValid')
--
parseRelFile :: MonadThrow m
             => FilePath -> m (Path Rel File)
parseRelFile filepath =
  if not (FilePath.isAbsolute filepath ||
          FilePath.hasTrailingPathSeparator filepath) &&
     not (null filepath) &&
     not (hasParentDir filepath) &&
     filepath /= "." && filepath /= ".." &&
     FilePath.isValid filepath
     then return (Path (normalizeFilePath filepath))
     else throwM (InvalidRelFile filepath)

-- | Helper function: check if the filepath has any parent directories in it.
-- This handles the logic of checking for different path separators on Windows.
hasParentDir :: FilePath -> Bool
hasParentDir filepath' =
     ("/.." `isSuffixOf` filepath) ||
     ("/../" `isInfixOf` filepath) ||
     ("../" `isPrefixOf` filepath)
  where
    filepath =
        case FilePath.pathSeparator of
            '/' -> filepath'
            x   -> map (\y -> if x == y then '/' else y) filepath'

--------------------------------------------------------------------------------
-- Constructors

-- | Make a 'Path Abs Dir'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsDir :: FilePath -> Q Exp
mkAbsDir s =
  case parseAbsDir s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Abs Dir|]

-- | Make a 'Path Rel Dir'.
mkRelDir :: FilePath -> Q Exp
mkRelDir s =
  case parseRelDir s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Rel Dir|]

-- | Make a 'Path Abs File'.
--
-- Remember: due to the nature of absolute paths this (e.g. @\/home\/foo@)
-- may compile on your platform, but it may not compile on another
-- platform (Windows).
mkAbsFile :: FilePath -> Q Exp
mkAbsFile s =
  case parseAbsFile s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Abs File|]

-- | Make a 'Path Rel File'.
mkRelFile :: FilePath -> Q Exp
mkRelFile s =
  case parseRelFile s of
    Left err -> error (show err)
    Right (Path str) ->
      [|Path $(return (LitE (StringL str))) :: Path Rel File|]

--------------------------------------------------------------------------------
-- Conversion

-- | Convert to a 'FilePath' type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b t -> FilePath
toFilePath (Path l) = l

-- | Convert absolute path to directory to 'FilePath' type.
fromAbsDir :: Path Abs Dir -> FilePath
fromAbsDir = toFilePath

-- | Convert relative path to directory to 'FilePath' type.
fromRelDir :: Path Rel Dir -> FilePath
fromRelDir = toFilePath

-- | Convert absolute path to file to 'FilePath' type.
fromAbsFile :: Path Abs File -> FilePath
fromAbsFile = toFilePath

-- | Convert relative path to file to 'FilePath' type.
fromRelFile :: Path Rel File -> FilePath
fromRelFile = toFilePath

--------------------------------------------------------------------------------
-- Operations

-- | Append two paths.
--
-- The following cases are valid and the equalities hold:
--
-- @$(mkAbsDir x) \<\/> $(mkRelDir y) = $(mkAbsDir (x ++ \"/\" ++ y))@
--
-- @$(mkAbsDir x) \<\/> $(mkRelFile y) = $(mkAbsFile (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelDir y) = $(mkRelDir (x ++ \"/\" ++ y))@
--
-- @$(mkRelDir x) \<\/> $(mkRelFile y) = $(mkRelFile (x ++ \"/\" ++ y))@
--
-- The following are proven not possible to express:
--
-- @$(mkAbsFile …) \<\/> x@
--
-- @$(mkRelFile …) \<\/> x@
--
-- @x \<\/> $(mkAbsFile …)@
--
-- @x \<\/> $(mkAbsDir …)@
--
(</>) :: Path b Dir -> Path Rel t -> Path b t
(</>) (Path a) (Path b) = Path (a ++ b)

-- | Strip directory from path, making it relative to that directory.
-- Throws 'InvalidPrefix' if directory is not a parent of the path.
--
-- The following properties hold:
--
-- @stripDir x (x \<\/> y) = y@
--
-- Cases which are proven not possible:
--
-- @stripDir (a :: Path Abs …) (b :: Path Rel …)@
--
-- @stripDir (a :: Path Rel …) (b :: Path Abs …)@
--
-- In other words the bases must match.
--
stripDir :: MonadThrow m
         => Path b Dir -> Path b t -> m (Path Rel t)
stripDir (Path p) (Path l) =
  case stripPrefix p l of
    Nothing -> throwM (InvalidPrefix p l)
    Just "" -> throwM (InvalidPrefix p l)
    Just ok -> return (Path ok)

-- | Determine if a directory is a parent of a given path.
--
-- The following properties hold:
--
-- @not (x `isParentOf` x)@
--
-- @x `isParentOf` (x \<\/\> y)@
--
isParentOf :: Path b Dir -> Path b t -> Bool
isParentOf p l =
  isJust (stripDir p l)

-- | Take the absolute parent directory from the absolute path.
--
-- The following properties hold:
--
-- @parent (x \<\/> y) == x@
--
-- Throws `HasNoParent` for root directory.
--
parent :: MonadThrow m
       => Path Abs t -> m (Path Abs Dir)
parent (Path fp) =
  case FilePath.isDrive fp of
    True  -> throwM (HasNoParent fp)
    False -> return $ Path
                    $ normalizeDir
                    $ FilePath.takeDirectory
                    $ FilePath.dropTrailingPathSeparator fp

-- | Extract the file part of a path.
--
-- The following properties hold:
--
-- @filename (p \<\/> a) == filename a@
--
filename :: Path b File -> Path Rel File
filename (Path l) =
  Path (FilePath.takeFileName l)

-- | Extract the last directory name of a path.
--
-- The following properties hold:
--
-- @dirname (p \<\/> a) == dirname a@
--
dirname :: Path b Dir -> Path Rel Dir
dirname (Path l) =
  Path (last (FilePath.splitPath l))

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
