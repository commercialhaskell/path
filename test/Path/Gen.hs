{-# LANGUAGE FlexibleInstances #-}
module Path.Gen where

import           Control.Applicative
import           Data.GenValidity
import           Data.Maybe
import           Data.Monoid
import           Data.List (isInfixOf)
import           Data.Validity
import           Prelude
import qualified System.FilePath as FilePath
import           Test.QuickCheck

import           Path
import           Path.Internal

instance Validity (Path Abs File) where
  isValid p@(Path fp)
    =  FilePath.isAbsolute fp
    && not (FilePath.hasTrailingPathSeparator fp)
    && FilePath.isValid fp
    && not (hasParentDir fp)
    && (parseAbsFile fp == Just p)

instance Validity (Path Rel File) where
  isValid p@(Path fp)
    =  FilePath.isRelative fp
    && not (FilePath.hasTrailingPathSeparator fp)
    && FilePath.isValid fp
    && fp /= "."
    && not (hasParentDir fp)
    && (parseRelFile fp == Just p)

instance Validity (Path Abs Dir) where
  isValid p@(Path fp)
    =  FilePath.isAbsolute fp
    && FilePath.hasTrailingPathSeparator fp
    && FilePath.isValid fp
    && not (hasParentDir fp)
    && (parseAbsDir fp == Just p)

instance Validity (Path Rel Dir) where
  isValid p@(Path fp)
    =  FilePath.isRelative fp
    && FilePath.hasTrailingPathSeparator fp
    && FilePath.isValid fp
    && not (null fp)
    && fp /= "."
    && not (hasParentDir fp)
    && (parseRelDir fp == Just p)

instance Arbitrary (Path Abs File) where
  arbitrary = genPathWith parseAbsFile
  shrink = shrinkValidAbsFile

instance Arbitrary (Path Rel File) where
  arbitrary = genPathWith parseRelFile
  shrink = shrinkValidRelFile

instance Arbitrary (Path Abs Dir) where
  arbitrary = genPathWith parseAbsDir
  shrink = shrinkValidAbsDir

instance Arbitrary (Path Rel Dir) where
  arbitrary = genPathWith parseRelDir
  shrink = shrinkValidRelDir

instance GenUnchecked (Path Abs File) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Abs File)

instance GenUnchecked (Path Rel File) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Rel File)

instance GenUnchecked (Path Abs Dir) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Abs Dir)

instance GenUnchecked (Path Rel Dir) where
  genUnchecked = Path <$> genFilePath

instance GenValid (Path Rel Dir)

-- | Generates 'FilePath's with a high occurence of @'.'@, @'\/'@ and
-- @'\\'@ characters.
genFilePath :: Gen FilePath
genFilePath = FilePath.makeValid <$> oneof
  [ listOf genPathyChar
  , do r <- genRoot
       x <- listOf genPathyChar
       pure (r FilePath.</> x)
  ]

-- | Generate root directories.
--
-- Results are not guaranteed to be valid.
genRoot :: Gen FilePath
genRoot = oneof
  [ pure "/"
  , (: ":") <$> arbitrary -- C:
  , ("\\\\" <>) <$> arbitrary -- \\foo
  , ("\\\\?\\UNC\\" <>) <$> arbitrary -- \\?\UNC\foo
  , (\c -> "\\\\?\\" <> (c : ":")) <$> arbitrary -- \\?\C:
  ]

genPathyChar :: Gen Char
genPathyChar = frequency [(2, arbitrary), (1, elements "./\\")]

shrinkValidAbsFile :: Path Abs File -> [Path Abs File]
shrinkValidAbsFile = shrinkValidWith parseAbsFile

shrinkValidAbsDir :: Path Abs Dir -> [Path Abs Dir]
shrinkValidAbsDir = shrinkValidWith parseAbsDir

shrinkValidRelFile :: Path Rel File -> [Path Rel File]
shrinkValidRelFile = shrinkValidWith parseRelFile

shrinkValidRelDir :: Path Rel Dir -> [Path Rel Dir]
shrinkValidRelDir = shrinkValidWith parseRelDir

shrinkValidWith :: (FilePath -> Maybe (Path a b)) -> Path a b -> [Path a b]
shrinkValidWith fun (Path s) = mapMaybe fun $ shrink s

fromMaybeGen :: Gen (Maybe a) -> Gen a
fromMaybeGen x = fmap fromJust (x `suchThat` isJust)

genPathWith :: (FilePath -> Maybe (Path b t)) -> Gen (Path b t)
genPathWith p = fromMaybeGen (p <$> genFilePath)
