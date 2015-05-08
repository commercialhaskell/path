{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | A normalizing well-typed path type.

module Path
  (-- * Types
   Path
  ,Abs
  ,Rel
  ,File
  ,Dir
   -- * Parsing
  ,parseAbsDir
  ,parseRelDir
  ,parseAbsFile
  ,parseRelFile
  ,PathParseException
  -- * Constructors
  ,mkAbsDir
  ,mkRelDir
  ,mkAbsFile
  ,mkRelFile
  -- * Conversion
  ,toFilePath
  )
  where

import           Control.Exception (Exception)
import           Control.Monad.Catch (MonadThrow(..))
import           Data.Data
import           Data.List
import           Language.Haskell.TH
import           Path.Internal
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------
-- Types

-- | An absolute path.
data Abs

-- | A relative path; one without a root.
data Rel

-- | A file path.
data File

-- | A directory path.
data Dir

-- | Exception when parsing a location.
data PathParseException
  = InvalidAbsDir FilePath
  | InvalidRelDir FilePath
  | InvalidAbsFile FilePath
  | InvalidRelFile FilePath
  deriving (Show,Typeable)
instance Exception PathParseException

--------------------------------------------------------------------------------
-- Parsers

-- | Get a location for an absolute directory. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseAbsDir :: MonadThrow m
            => FilePath -> m (Path Abs Dir)
parseAbsDir filepath =
  if FilePath.isAbsolute filepath &&
     not (null (normalizeDir filepath)) &&
     not (isPrefixOf "~/" filepath)
     then return (Path (normalizeDir filepath))
     else throwM (InvalidAbsDir filepath)

-- | Get a location for a relative directory. Produces a normalized
-- path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseRelDir :: MonadThrow m
            => FilePath -> m (Path Rel Dir)
parseRelDir filepath =
  if not (FilePath.isAbsolute filepath) &&
     not (null filepath) &&
     not (isPrefixOf "~/" filepath) &&
     not (null (normalizeDir filepath))
     then return (Path (normalizeDir filepath))
     else throwM (InvalidRelDir filepath)

-- | Get a location for an absolute file. Produces a normalized
--  path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseAbsFile :: MonadThrow m
             => FilePath -> m (Path Abs File)
parseAbsFile filepath =
  if FilePath.isAbsolute filepath &&
     not (FilePath.hasTrailingPathSeparator filepath) &&
     not (isPrefixOf "~/" filepath) &&
     not (null (normalizeFile filepath))
     then return (Path (normalizeFile filepath))
     else throwM (InvalidAbsFile filepath)

-- | Get a location for a relative file. Produces a normalized
-- path which always ends in a path separator.
--
-- Throws: 'PathParseException'
--
parseRelFile :: MonadThrow m
             => FilePath -> m (Path Rel File)
parseRelFile filepath =
  if not (FilePath.isAbsolute filepath || FilePath.hasTrailingPathSeparator filepath) &&
     not (null filepath) &&
     not (isPrefixOf "~/" filepath) &&
     not (null (normalizeFile filepath))
     then return (Path (normalizeFile filepath))
     else throwM (InvalidRelFile filepath)

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
toFilePath :: Path b t -> FilePath
toFilePath (Path l) = l

--------------------------------------------------------------------------------
-- Internal functions

-- | Internal use for normalizing a directory.
normalizeDir :: FilePath -> FilePath
normalizeDir =
  clean . FilePath.addTrailingPathSeparator . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x

-- | Internal use for normalizing a fileectory.
normalizeFile :: FilePath -> FilePath
normalizeFile =
  clean . FilePath.normalise
  where clean "./" = ""
        clean ('/':'/':xs) = clean ('/':xs)
        clean x = x
