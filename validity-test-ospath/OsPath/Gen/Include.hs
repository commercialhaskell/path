{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OsPath.Gen.PLATFORM_NAME where

import Data.Functor
import Prelude

import OsPath.PLATFORM_NAME
import OsPath.Internal.PLATFORM_NAME

import Data.GenValidity
import Data.Maybe (mapMaybe)
import Data.Word (PLATFORM_WORD)
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import System.OsString.Internal.Types (PLATFORM_CHAR(..))
import qualified System.OsString.PLATFORM_NAME as OsString
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
          parseRelDir fp == Just p || OsString.null fp
      ]

instance Validity (SomeBase Dir)

instance Validity (SomeBase File)

instance GenValid (Path Abs File) where
  genValid = (Path . ([OsString.pstr|/|] <>) <$> genValid) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseAbsFile

instance GenValid (Path Abs Dir) where
  genValid = (Path . ([OsString.pstr|/|] <>) . (<> OsString.singleton OsPath.pathSeparator) <$> genValid) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseAbsDir

instance GenValid (Path Rel File) where
  genValid = (Path <$> genValid) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseRelFile

instance GenValid (Path Rel Dir) where
  genValid = (Path . (<> OsString.singleton OsPath.pathSeparator) <$> genValid) `suchThat` isValid
  shrinkValid = filter isValid . shrinkValidWith parseRelDir

instance GenValid (SomeBase Dir) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid (SomeBase File) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

validateCommon :: Path b t -> Validation
validateCommon (Path fp) = mconcat
  [ declare "System.FilePath considers the path valid if it's not empty." $
      OsPath.isValid fp || OsString.null fp
  , declare "The path does not contain a '..' path component." $
      not (hasParentDir fp)
  ]

validateDirectory :: Path b Dir -> Validation
validateDirectory (Path fp) = mconcat
  [ declare "The path has a trailing path separator if it's not empty." $
      OsPath.hasTrailingPathSeparator fp || OsString.null fp
  ]

validateFile :: Path b File -> Validation
validateFile (Path fp) = mconcat
  [ declare "The path has no trailing path separator." $
      not (OsPath.hasTrailingPathSeparator fp)
  , declare "The path does not equal \".\"" $
      fp /= [OsString.pstr|.|]
  , declare "The path does not end in /." $
      not ([OsString.pstr|/.|] `OsString.isSuffixOf` fp)
  ]

validateAbs :: Path Abs t -> Validation
validateAbs (Path fp) = mconcat
  [ declare "The path is absolute." $
      OsPath.isAbsolute fp
  ]

validateRel :: Path Rel t -> Validation
validateRel (Path fp) = mconcat
  [ declare "The path is relative." $
      OsPath.isRelative fp
  ]

shrinkValidWith :: (PLATFORM_PATH -> Maybe (Path a b)) -> Path a b -> [Path a b]
shrinkValidWith fun (Path f) = filter (/= Path f) . mapMaybe fun $ shrinkValid f

--------------------------------------------------------------------------------
-- Orphan instances

deriving via PLATFORM_WORD instance GenValid PLATFORM_CHAR
deriving via PLATFORM_WORD instance Validity PLATFORM_CHAR

-- | Generates PLATFORM_PATH_SINGLE with a high occurence of
-- 'OsPath.extSeparator' and 'OsPath.pathSeparators' characters. The resulting
-- paths are not guaranteed to be valid in the sense of 'OsPath.isValid'.
instance GenValid PLATFORM_PATH where
    genValid = OsPath.pack <$> listOf (frequency
        [ (2, genValid)
        , (1, elements (OsPath.extSeparator : OsPath.pathSeparators))
        ]
        )
    shrinkValid ospath =
      let (drive, relative) = OsPath.splitDrive ospath
          shrinkedWithoutDrive =
              map OsPath.pack
            . shrinkValid
            . OsPath.unpack
            $ relative
          shrinkedWithDrive =
            if OsString.null drive
            then []
            else map (drive <>) shrinkedWithoutDrive
      in
      shrinkedWithDrive <> shrinkedWithoutDrive

instance Validity PLATFORM_PATH where
    validate = trivialValidation -- TODO: Not yet implemented
