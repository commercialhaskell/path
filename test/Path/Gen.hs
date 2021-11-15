{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Path.Gen where

import Data.Functor
import Prelude

import Path
import Path.Internal

import qualified System.FilePath as FilePath

import Data.GenValidity
import Data.List (isSuffixOf, isInfixOf)
import Data.Maybe (isJust, mapMaybe)

import Test.QuickCheck

instance Validity (Path Abs File) where
  validate p@(Path fp) =
    mconcat
      [ validateCommon p,
        validateAbs p,
        validateFile p,
        declare "The path can be identically parsed as an absolute file path." $
          parseAbsFile fp == Just p
      ]

instance Validity (Path Rel File) where
  validate p@(Path fp) =
    mconcat
      [ validateCommon p,
        validateRel p,
        validateFile p,
        declare "The path can be identically parsed as a relative file path." $
          parseRelFile fp == Just p
      ]

instance Validity (Path Abs Dir) where
  validate p@(Path fp) =
    mconcat
      [ validateCommon p,
        validateAbs p,
        validateDirectory p,
        declare "The path can be identically parsed as an absolute directory path." $
          parseAbsDir fp == Just p
      ]

instance Validity (Path Rel Dir) where
  validate p@(Path fp) =
    mconcat
      [ validateCommon p,
        validateRel p,
        validateDirectory p,
        declare "The path can be identically parsed as a relative directory path if it's not empty." $
          parseRelDir fp == Just p || fp == ""
      ]

instance Validity (SomeBase Dir)

instance Validity (SomeBase File)

validateCommon :: Path b t -> Validation
validateCommon (Path fp) = mconcat
  [ declare "System.FilePath considers the path valid if it's not empty." $ FilePath.isValid fp || fp == ""
  , declare "The path does not contain a '..' path component." $ not (hasParentDir fp)
  ]

validateDirectory :: Path b Dir -> Validation
validateDirectory (Path fp) = mconcat
  [ declare "The path has a trailing path separator if it's not empty." $ FilePath.hasTrailingPathSeparator fp || fp == ""
  ]

validateFile :: Path b File -> Validation
validateFile (Path fp) = mconcat
  [ declare "The path has no trailing path separator." $ not (FilePath.hasTrailingPathSeparator fp)
  , declare "The path does not equal \".\"" $ fp /= "."
  , declare "The path does not end in /." $ not ("/." `isSuffixOf` fp)
  ]

validateAbs :: Path Abs t -> Validation
validateAbs (Path fp) = mconcat
  [ declare "The path is absolute." $ FilePath.isAbsolute fp
  ]

validateRel :: Path Rel t -> Validation
validateRel (Path fp) = mconcat
  [ declare "The path is relative." $ FilePath.isRelative fp
  ]

instance GenValid (Path Abs File) where
  genValid = (Path . ('/' :) <$> genFilePath) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseAbsFile

instance GenValid (Path Abs Dir) where
  genValid = (Path . ('/' :) . (++ "/") <$> genFilePath) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseAbsDir

instance GenValid (Path Rel File) where
  genValid = (Path <$> genFilePath) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseRelFile

instance GenValid (Path Rel Dir) where
  genValid = (Path . (++ "/") <$> genFilePath) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseRelDir

instance GenValid (SomeBase Dir) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid (SomeBase File) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

-- | Generates 'FilePath's with a high occurence of @'.'@, @'\/'@ and
-- @'\\'@ characters. The resulting 'FilePath's are not guaranteed to
-- be valid.
genFilePath :: Gen FilePath
genFilePath = listOf genPathyChar

genPathyChar :: Gen Char
genPathyChar = frequency [(2, choose (minBound, maxBound)), (1, elements "./\\")]

shrinkValidWith :: (FilePath -> Maybe (Path a b)) -> Path a b -> [Path a b]
shrinkValidWith fun (Path f) = filter (/= (Path f)) . mapMaybe fun $ shrinkValid f
