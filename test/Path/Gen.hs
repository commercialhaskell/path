{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Path.Gen where

import           Data.Functor
import           Prelude

import           Path
import           Path.Internal

import qualified System.FilePath as FilePath

import           Data.Maybe (mapMaybe, isJust)
import           Data.List (isInfixOf)
import           Data.Validity
import           Data.GenValidity

import           Test.QuickCheck


instance Validity (Path Abs File) where
  validate p@(Path fp) =  check (FilePath.isAbsolute fp)                     "Path is absolute."
                       <> check (not (FilePath.hasTrailingPathSeparator fp)) "Path does not have trailing seperator."
                       <> check (FilePath.isValid fp)                        "Path is valid."
                       <> check (not (hasParentDir fp))                      "Path does not have parent dir."
                       <> check ((parseAbsFile fp == Just p))                "Path parses."

instance Validity (Path Rel File) where
  validate p@(Path fp) =  check (FilePath.isRelative fp)                     "Path is relative."
                       <> check (not (FilePath.hasTrailingPathSeparator fp)) "Path does not have trailing seperator."
                       <> check (FilePath.isValid fp)                        "Path is valid."
                       <> check (fp /= ".")                                  "Path is not '.' ."
                       <> check (not (hasParentDir fp))                      "Path does not have parent dir."
                       <> check ((parseRelFile fp == Just p))                "Path parses."

instance Validity (Path Abs Dir) where
  validate p@(Path fp) =  check (FilePath.isAbsolute fp)               "Path is absolute."
                       <> check (FilePath.hasTrailingPathSeparator fp) "Path has trailing seperator."
                       <> check (FilePath.isValid fp)                  "Path is valid."
                       <> check (not (hasParentDir fp))                 "Path does not have parent dir."
                       <> check ((parseAbsDir fp == Just p))           "Path parses."
                         
instance Validity (Path Rel Dir) where
  validate p =  check (FilePath.isRelative fp)               "Path is relative."
             <> check (FilePath.hasTrailingPathSeparator fp) "Path has trailing seperator"
             <> check (FilePath.isValid fp)                  "Path is valid."
             <> check (not (hasParentDir fp))                "Path does not have parent dir."
             <> check ((parseRelDir fp == Just p))           "Path parses."
    where fp = toFilePath p

instance GenUnchecked (Path Abs File) where
  genUnchecked = Path <$> genFilePath
  shrinkUnchecked (Path p) = Path <$> shrink p

instance GenValid (Path Abs File)

instance GenUnchecked (Path Rel File) where
  genUnchecked = Path <$> genFilePath
  shrinkUnchecked (Path p) = Path <$> shrink p

instance GenValid (Path Rel File)

instance GenUnchecked (Path Abs Dir) where
  genUnchecked = Path <$> genFilePath
  shrinkUnchecked (Path p) = Path <$> shrink p

instance GenValid (Path Abs Dir)

instance GenUnchecked (Path Rel Dir) where
  genUnchecked = Path <$> genFilePath
  shrinkUnchecked (Path p) = Path <$> shrink p

instance GenValid (Path Rel Dir)

data Extension = Extension String deriving Show

instance Validity Extension where
  validate (Extension ext) = check (isJust $ addExtension ext $(mkRelFile "x")) "Extension is valid."

instance GenUnchecked Extension where
  genUnchecked = Extension <$> genFilePath
  shrinkUnchecked (Extension ext) = Extension <$> shrink ext

instance GenValid Extension

-- | Generates 'FilePath's with a high occurence of @'.'@, @'\/'@ and
-- @'\\'@ characters. The resulting 'FilePath's are not guaranteed to
-- be valid.
genFilePath :: Gen FilePath
genFilePath = listOf genPathyChar

genPathyChar :: Gen Char
genPathyChar = frequency [(2, arbitrary), (1, elements "./\\")]

shrinkValidAbsFile :: Path Abs File -> [Path Abs File]
shrinkValidAbsFile = shrinkValidWith parseAbsFile

shrinkValidAbsDir :: Path Abs Dir -> [Path Abs Dir]
shrinkValidAbsDir = shrinkValidWith parseAbsDir

shrinkValidRelFile :: Path Rel File -> [Path Rel File]
shrinkValidRelFile = shrinkValidWith parseRelFile

shrinkValidRelDir :: Path Rel Dir -> [Path Rel Dir]
shrinkValidRelDir (Path []) = []
shrinkValidRelDir p = shrinkValidWith parseRelDir p

shrinkValidWith :: (FilePath -> Maybe (Path a b)) -> Path a b -> [Path a b]
shrinkValidWith fun (Path s) = mapMaybe fun $ shrink s

shrinkValidExtension :: Extension -> [Extension]
shrinkValidExtension (Extension s) =
    map (Extension . drop 1 . toFilePath) $
        mapMaybe (flip addExtension $(mkRelFile "x")) (shrink s)
