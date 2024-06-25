-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_PATH_SINGLE = 'PosixPath' | 'WindowsPath'
--     IS_WINDOWS = 0 | 1

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal types and functions.

module OsPath.Internal.PLATFORM_NAME
  ( -- * The Path type
    Path(..)
  , toOsPath

    -- * Validation functions
  , isValidAbsDir
  , isValidAbsFile
  , isValidRelDir
  , isValidRelFile
  , hasParentDir

    -- * Normalizing functions
  , normalizeLeadingSeps
  , normalizeTrailingSeps
  , normalizeAllSeps
#if IS_WINDOWS
  , normalizeWindowsSeps
#endif
  , normalizeDrive
  , normalizeDir
  , normalizeFile

    -- * Other helper functions
  , extSep
  , pathSep
  , relRoot
  , isWindows
  )
  where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)
import Data.Data
import Data.Hashable
import qualified Language.Haskell.TH.Syntax as TH
import System.OsPath.PLATFORM_NAME (PLATFORM_PATH)
import qualified System.OsPath.PLATFORM_NAME as OsPath
import System.OsString.Internal.Types (PLATFORM_STRING(..))
import qualified System.OsString.PLATFORM_NAME as OsString

-- | Path of some base and type.
--
-- The type variables are:
--
--   * @b@ — base, the base location of the path; absolute or relative.
--   * @t@ — type, whether file or directory.
--
-- Internally it is a PLATFORM_PATH_SINGLE, which can be of two formats only:
--
-- 1. File format: @file.txt@, @foo\/bar.txt@, @\/foo\/bar.txt@
-- 2. Directory format: @foo\/@, @\/foo\/bar\/@
--
-- All directories end in a trailing separator. There are no duplicate
-- path separators @\/\/@, no @..@, no @.\/@, no @~\/@, etc.
newtype Path b t = Path PLATFORM_PATH
  deriving (Data, Typeable, Generic)

-- | String equality.
--
-- The following property holds:
--
-- @show x == show y ≡ x == y@
instance Eq (Path b t) where
  (==) (Path x) (Path y) = x == y
  {-# INLINE (==) #-}

-- | String ordering.
--
-- The following property holds:
--
-- @show x \`compare\` show y ≡ x \`compare\` y@
instance Ord (Path b t) where
  compare (Path x) (Path y) = compare x y
  {-# INLINE compare #-}

-- | Same as 'show . OsPath.toOsPath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b t) where
  show = show . toOsPath
  {-# INLINE show #-}

instance NFData (Path b t) where
  rnf (Path x) = rnf x
  {-# INLINE rnf #-}

instance Hashable (Path b t) where
  -- A "." is represented as an empty string ("") internally. Hashing ""
  -- results in a hash that is the same as the salt. To produce a more
  -- reasonable hash we use "toFilePath" before hashing so that a "" gets
  -- converted back to a ".".
  hashWithSalt n path = hashWithSalt n (toOsPath path)
  {-# INLINE hashWithSalt #-}

instance forall b t. (Typeable b, Typeable t) => TH.Lift (Path b t) where
  lift (Path str) = do
    let b = TH.ConT $ getTCName (Proxy :: Proxy b)
        t = TH.ConT $ getTCName (Proxy :: Proxy t)
    [| Path $(TH.lift str) :: Path $(pure b) $(pure t) |]
    where
      getTCName :: Typeable a => proxy a -> TH.Name
      getTCName a = TH.Name occ flav
        where
        tc   = typeRepTyCon (typeRep a)
        occ  = TH.OccName (tyConName tc)
        flav = TH.NameG TH.TcClsName (TH.PkgName (tyConPackage tc)) (TH.ModName (tyConModule tc))

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | Convert to a PLATFORM_PATH type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'OsPath.dropTrailingPathSeparator' from
-- the filepath package.
toOsPath :: Path b t -> PLATFORM_PATH
toOsPath (Path ospath)
    | OsString.null ospath = relRoot
    | otherwise = ospath

--------------------------------------------------------------------------------
-- Validation functions

-- | Is the PLATFORM_PATH_SINGLE a valid absolute dir?
isValidAbsDir :: PLATFORM_PATH -> Bool
isValidAbsDir ospath =
  OsPath.isAbsolute ospath &&
  not (hasParentDir ospath) &&
  OsPath.isValid ospath

-- | Is the PLATFORM_PATH_SINGLE a valid absolute file?
isValidAbsFile :: PLATFORM_PATH -> Bool
isValidAbsFile ospath =
  OsPath.isAbsolute ospath &&
  not (OsPath.hasTrailingPathSeparator ospath) &&
  not (hasParentDir ospath) &&
  OsPath.isValid ospath

-- | Is the PLATFORM_PATH_SINGLE a valid relative dir?
isValidRelDir :: PLATFORM_PATH -> Bool
isValidRelDir ospath =
  not (OsPath.isAbsolute ospath) &&
  not (OsString.null ospath) &&
  not (hasParentDir ospath) &&
  not (OsString.all OsPath.isPathSeparator ospath) &&
  OsPath.isValid ospath

-- | Is the PLATFORM_PATH_SINGLE a valid relative file?
isValidRelFile :: PLATFORM_PATH -> Bool
isValidRelFile ospath =
  not (OsPath.isAbsolute ospath) &&
  not (OsString.null ospath) &&
  not (hasParentDir ospath) &&
  not (OsPath.hasTrailingPathSeparator ospath) &&
  ospath /= [OsPath.pstr|.|] &&
  OsPath.isValid ospath

-- | Helper function: check if the filepath has any parent directories in it.
-- This handles the logic of checking for different path separators on Windows.
hasParentDir :: PLATFORM_PATH -> Bool
hasParentDir ospath =
     (ospath' == [OsString.pstr|..|]) ||
     (prefix' `OsString.isPrefixOf` ospath') ||
     (infix' `OsString.isInfixOf` ospath') ||
     (suffix' `OsString.isSuffixOf` ospath')
  where
    prefix' = [OsString.pstr|..|] <> pathSep
    infix' = pathSep <> [OsString.pstr|..|] <> pathSep
    suffix' = pathSep <> [OsString.pstr|..|]

#if IS_WINDOWS
    ospath' = OsString.map normSep ospath
    normSep c
      | OsPath.isPathSeparator c = OsPath.pathSeparator
      | otherwise = c
#else
    ospath' = ospath
#endif

--------------------------------------------------------------------------------
-- Normalizing functions

-- | Normalizes seps only at the beginning of a path.
normalizeLeadingSeps :: PLATFORM_PATH -> PLATFORM_PATH
normalizeLeadingSeps path = normLeadingSep <> rest
  where (leadingSeps, rest) = OsString.span OsPath.isPathSeparator path
        normLeadingSep
          | OsString.null leadingSeps = OsString.empty
          | otherwise = OsString.singleton OsPath.pathSeparator

-- | Normalizes seps only at the end of a path.
normalizeTrailingSeps :: PLATFORM_PATH -> PLATFORM_PATH
normalizeTrailingSeps path = rest <> normTrailingSep
  where (rest, trailingSeps) = OsString.spanEnd OsPath.isPathSeparator path
        normTrailingSep
          | OsString.null trailingSeps = OsString.empty
          | otherwise = OsString.singleton OsPath.pathSeparator

-- | Replaces consecutive path seps with single sep and replaces alt sep with
--   standard sep.
normalizeAllSeps :: PLATFORM_PATH -> PLATFORM_PATH
normalizeAllSeps = go OsString.empty
  where go !acc ospath
          | OsString.null ospath = acc
          | otherwise =
            let (leadingSeps, withoutLeadingSeps) =
                  OsString.span OsPath.isPathSeparator ospath
                (name, rest) =
                  OsString.break OsPath.isPathSeparator withoutLeadingSeps
                sep = if OsString.null leadingSeps
                      then OsString.empty
                      else OsString.singleton OsPath.pathSeparator
            in go (acc <> sep <> name) rest

#if IS_WINDOWS
-- | Normalizes seps in whole path, but if there are 2+ seps at the beginning,
--   they are normalized to exactly 2 to preserve UNC and Unicode prefixed
--   paths.
normalizeWindowsSeps :: PLATFORM_PATH -> PLATFORM_PATH
normalizeWindowsSeps path = normLeadingSeps <> normalizeAllSeps rest
  where (leadingSeps, rest) = OsString.span OsPath.isPathSeparator path
        normLeadingSeps = OsString.replicate
          (min 2 (OsString.length leadingSeps))
          OsPath.pathSeparator
#endif

-- | Normalizes the drive of a PLATFORM_PATH_SINGLE.
normalizeDrive :: PLATFORM_PATH -> PLATFORM_PATH
#if IS_WINDOWS
normalizeDrive = normalizeTrailingSeps
#else
normalizeDrive = id
#endif

-- | Normalizes directory path with platform-specific rules.
normalizeDir :: PLATFORM_PATH -> PLATFORM_PATH
normalizeDir =
      normalizeRelDir
    . OsPath.addTrailingPathSeparator
    . normalizeFile
  where -- Represent a "." in relative dir path as "" internally so that it
        -- composes without having to renormalize the path.
        normalizeRelDir p
          | p == relRoot = OsString.empty
          | otherwise = p

-- | Applies platform-specific sep normalization following @OsPath.normalise@.
normalizeFile :: PLATFORM_PATH -> PLATFORM_PATH
#if IS_WINDOWS
normalizeFile = normalizeWindowsSeps . OsPath.normalise
#else
normalizeFile = normalizeLeadingSeps . OsPath.normalise
#endif

--------------------------------------------------------------------------------
-- Other helper functions

extSep :: PLATFORM_STRING
extSep = $(TH.lift (OsString.singleton OsPath.extSeparator))

pathSep :: PLATFORM_STRING
pathSep = $(TH.lift (OsString.singleton OsPath.pathSeparator))

-- | Normalized file path representation for the relative path root
relRoot :: PLATFORM_PATH
relRoot = $(TH.lift ([OsPath.pstr|.|] <> OsString.singleton OsPath.pathSeparator))

isWindows :: Bool
#if IS_WINDOWS
isWindows = True
#else
isWindows = False
#endif
{-# INLINE isWindows #-}

--------------------------------------------------------------------------------
-- Orphan instances

deriving instance Data PLATFORM_STRING
