{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Path.Gen where

import Data.Functor
import Prelude

import Path
import Path.Internal

import qualified System.FilePath as FilePath

import Data.GenValidity
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe (isJust, mapMaybe)
import Data.Validity

import Test.QuickCheck

-- | An absolute path to a file is valid if:
--
-- * Its path is an absolute path
-- * Its path has no trailing path separators
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not end in '/.'
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs File) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is absolute." $ FilePath.isAbsolute fp
      , declare "The path has no trailing path separator." $
        not (FilePath.hasTrailingPathSeparator fp)
      , declare "System.FilePath considers the path valid." $ FilePath.isValid fp
      , declare "The path does not end in /." $ not ("/." `isSuffixOf` fp)
      , declare "The path does not equal \".\"" $ fp /= "."
      , declare "The path does not a parent directory." $ not (hasParentDir fp)
      , declare "The path can be identically parsed as an absolute file path." $
        parseAbsFile fp == Just p
      ]

-- | A relative path to a file is valid if:
--
-- * Its path is a relative path
-- * Its path does not have a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path is not '.'
-- * Its path is not empty
-- * Its path does not end in '/.'
-- * Its path is not '.'
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel File) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is relative." $ FilePath.isRelative fp
      , declare "The path has no trailing path separator." $
        not (FilePath.hasTrailingPathSeparator fp)
      , declare "System.FilePath considers the path valid." $ FilePath.isValid fp
      , declare "The path does not equal \".\"" $ fp /= "."
      , declare "The path is not empty" $ not (null fp)
      , declare "The path does not end in /." $ not ("/." `isSuffixOf` fp)
      , declare "The path does not a parent directory." $ not (hasParentDir fp)
      , declare "The path can be identically parsed as a relative file path." $
        parseRelFile fp == Just p
      ]

-- | An absolute path to a directory is valid if:
--
-- * Its path is an absolute path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Abs Dir) where
  validate p@(Path fp) =
    mconcat
      [ declare "The path is absolute." $ FilePath.isAbsolute fp
      , declare "The path has a trailing path separator." $ FilePath.hasTrailingPathSeparator fp
      , declare "System.FilePath considers the path valid." $ FilePath.isValid fp
      , declare "The path does not a parent directory." $ not (hasParentDir fp)
      , declare "The path can be identically parsed as an absolute directory path." $
        parseAbsDir fp == Just p
      ]

-- | A relative path to a directory is valid if:
--
-- * Its path is a relative path
-- * Its path has a trailing path separator
-- * Its path is valid according to 'System.FilePath's definition.
-- * Its path does not contain '..'.
-- * Parsing the path and rendering it again results in the same path.
instance Validity (Path Rel Dir) where
  validate (Path "") = valid
  validate p@(Path fp) =
    mconcat
      [ declare "The path is relative." $ FilePath.isRelative fp
      , declare "The path has a trailing path separator." $ FilePath.hasTrailingPathSeparator fp
      , declare "System.FilePath considers the path valid." $ FilePath.isValid fp
      , declare "The path is not empty." $ not (null fp)
      , declare "The path does not a parent directory." $ not (hasParentDir fp)
      , declare "The path can be identically parsed as a relative directory path." $
        parseRelDir fp == Just p
      ]

instance GenUnchecked (Path Abs File) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Abs File) where
  shrinkValid = shrinkValidWith parseAbsFile

instance GenUnchecked (Path Rel File) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Rel File) where
  shrinkValid = shrinkValidWith parseRelFile

instance GenUnchecked (Path Abs Dir) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Abs Dir) where
  shrinkValid = shrinkValidWith parseAbsDir

instance GenUnchecked (Path Rel Dir) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Rel Dir) where
  shrinkValid = shrinkValidWith parseRelDir

data Extension =
  Extension String
  deriving (Show)

instance Validity Extension where
  validate (Extension ext) =
    mconcat
      [ delve "Extension" ext
      , declare "It is possible to add the extension to \"./\"" $
        isJust $ addExtension ext $(mkRelFile "x")
      ]

instance GenUnchecked Extension where
  genUnchecked = Extension <$> genFilePath
  shrinkUnchecked (Extension e) = Extension <$> shrinkUnchecked e

instance GenValid Extension

-- | Generates 'FilePath's with a high occurence of @'.'@, @'\/'@ and
-- @'\\'@ characters. The resulting 'FilePath's are not guaranteed to
-- be valid.
genFilePath :: Gen FilePath
genFilePath = listOf genPathyChar

genPathyChar :: Gen Char
genPathyChar = frequency [(2, choose (minBound, maxBound)), (1, elements "./\\")]

shrinkValidWith :: (FilePath -> Maybe (Path a b)) -> Path a b -> [Path a b]
shrinkValidWith fun (Path f) = filter (/= (Path f)) . mapMaybe fun $ shrinkUnchecked f

shrinkValidExtension :: Extension -> [Extension]
shrinkValidExtension (Extension s) =
  map (Extension . drop 1 . toFilePath) $ mapMaybe (flip addExtension $(mkRelFile "x")) (shrink s)
