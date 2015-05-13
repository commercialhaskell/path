{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal types and functions.

module Path.Internal
  (Path(..))
  where

import Data.Data

-- | Path of some base and type.
--
-- Internally is a string. The string can be of two formats only:
--
-- 1. File format: @file.txt@, @foo\/bar.txt@, @\/foo\/bar.txt@
-- 2. Directory format: @foo\/@, @\/foo\/bar\/@
--
-- All directories end in a trailing separator. There are no duplicate
-- path separators @\/\/@, no @..@, no @.\/@, no @~\/@, etc.
newtype Path b t = Path FilePath
  deriving (Typeable)

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

-- | Same as 'Path.toFilePath'.
--
-- The following property holds:
--
-- @x == y ≡ show x == show y@
instance Show (Path b t) where
  show (Path x) = show x
