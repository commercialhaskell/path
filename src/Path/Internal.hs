{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Internal types and functions.

module Path.Internal
  ( Path(..)
  , hasParentDir
  , relRootFP
  , toFilePath
  )
  where

import Control.DeepSeq (NFData (..))
import Data.Aeson (ToJSON (..), ToJSONKey(..))
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T (pack)
import GHC.Generics (Generic)
import Data.Data
import Data.Hashable
import qualified Data.List as L
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.FilePath as FilePath

-- | Path of some base and type.
--
-- The type variables are:
--
--   * @b@ — base, the base location of the path; absolute or relative.
--   * @t@ — type, whether file or directory.
--
-- Internally is a string. The string can be of two formats only:
--
-- 1. File format: @file.txt@, @foo\/bar.txt@, @\/foo\/bar.txt@
-- 2. Directory format: @foo\/@, @\/foo\/bar\/@
--
-- All directories end in a trailing separator. There are no duplicate
-- path separators @\/\/@, no @..@, no @.\/@, no @~\/@, etc.
newtype Path b t = Path FilePath
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

-- | Normalized file path representation for the relative path root
relRootFP :: FilePath
relRootFP = '.' : [FilePath.pathSeparator]

-- | Convert to a 'FilePath' type.
--
-- All directories have a trailing slash, so if you want no trailing
-- slash, you can use 'System.FilePath.dropTrailingPathSeparator' from
-- the filepath package.
toFilePath :: Path b t -> FilePath
toFilePath (Path []) = relRootFP
toFilePath (Path x)  = x

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
  toJSONKey = toJSONKeyText $ T.pack . toFilePath

instance Hashable (Path b t) where
  -- A "." is represented as an empty string ("") internally. Hashing ""
  -- results in a hash that is the same as the salt. To produce a more
  -- reasonable hash we use "toFilePath" before hashing so that a "" gets
  -- converted back to a ".".
  hashWithSalt n path = hashWithSalt n (toFilePath path)

-- | Helper function: check if the filepath has any parent directories in it.
-- This handles the logic of checking for different path separators on Windows.
hasParentDir :: FilePath -> Bool
hasParentDir filepath' =
     (filepath' == "..") ||
     ("/.." `L.isSuffixOf` filepath) ||
     ("/../" `L.isInfixOf` filepath) ||
     ("../" `L.isPrefixOf` filepath)
  where
    filepath =
        case FilePath.pathSeparator of
            '/' -> filepath'
            x   -> map (\y -> if x == y then '/' else y) filepath'

instance TH.Lift (Path a b) where
  lift (Path str) = [|Path $(return (TH.LitE (TH.StringL str)))|]
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif
