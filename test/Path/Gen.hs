{-# LANGUAGE FlexibleInstances #-}
module Path.Gen where

import           Path
import           Path.Internal

import qualified System.FilePath as FilePath

import           Data.Validity
import           Data.GenValidity

import           Test.QuickCheck


instance Validity (Path Abs File) where
  isValid (Path fp)
    =  FilePath.isAbsolute fp
    && not (FilePath.hasTrailingPathSeparator fp)

instance Validity (Path Rel File) where
  isValid (Path fp)
    =  FilePath.isRelative fp
    && not (FilePath.hasTrailingPathSeparator fp)

instance Validity (Path Abs Dir) where
  isValid (Path fp)
    =  FilePath.isAbsolute fp
    && FilePath.hasTrailingPathSeparator fp

instance Validity (Path Rel Dir) where
  isValid (Path fp)
    =  FilePath.isRelative fp
    && FilePath.hasTrailingPathSeparator fp


instance GenValidity (Path Abs File) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Rel File) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Abs Dir) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Rel Dir) where
  genUnchecked = Path <$> arbitrary

