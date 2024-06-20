-- This template expects CPP definitions for:
--     PLATFORM_NAME = Posix | Windows
--     PLATFORM_PATH = PosixPath | WindowsPath
--     PLATFORM_PATH_SINGLE = 'PosixPath' | 'WindowsPath'
--     IS_WINDOWS = 0 | 1

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Internal types and functions.

module OsPath.Internal.PLATFORM_NAME
  ( Path(..)
  , toFilePath
  , toOsPath

    -- * Other helper functions
  , extSep
  , pathSep
  , hasParentDir
  , relRoot
  , isWindows
  )
  where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), ToJSONKey(..))
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as Text (pack)
import GHC.Generics (Generic)
import Data.Data
import Data.Hashable
import qualified Language.Haskell.TH.Syntax as TH
import System.IO.Unsafe (unsafeDupablePerformIO)
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

-- | String ordering.
--
-- The following property holds:
--
-- @show x \`compare\` show y ≡ x \`compare\` y@
instance Ord (Path b t) where
  compare (Path x) (Path y) = compare x y

-- | Convert to a 'FilePath' type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b t -> FilePath
toFilePath = unsafeDupablePerformIO . OsPath.decodeFS . toOsPath

-- | Convert to a PLATFORM_PATH type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'OsPath.dropTrailingPathSeparator' from
-- the filepath package.
toOsPath :: Path b t -> PLATFORM_PATH
toOsPath (Path ospath)
    | OsString.null ospath = relRoot
    | otherwise = ospath

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

-- | Same as 'show . Path.toFilePath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b t) where
  show = show . toFilePath

instance NFData (Path b t) where
  rnf (Path x) = rnf x

instance ToJSON (Path b t) where
  toJSON = toJSON . toFilePath
  {-# INLINE toJSON #-}
#if MIN_VERSION_aeson(0,10,0)
  toEncoding = toEncoding . toFilePath
  {-# INLINE toEncoding #-}
#endif

instance ToJSONKey (Path b t) where
  toJSONKey = toJSONKeyText (Text.pack . toFilePath)

instance Hashable (Path b t) where
  -- A "." is represented as an empty string ("") internally. Hashing ""
  -- results in a hash that is the same as the salt. To produce a more
  -- reasonable hash we use "toFilePath" before hashing so that a "" gets
  -- converted back to a ".".
  hashWithSalt n path = hashWithSalt n (toFilePath path)

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

--------------------------------------------------------------------------------
-- Orphan instances

deriving instance Data PLATFORM_STRING