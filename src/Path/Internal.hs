{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Internal types and functions.

module Path.Internal
  (Path(..))
  where

import Data.Data
import GHC.Generics

-- | Path of some base and type.
newtype Path b t = Path FilePath
  deriving (Eq,Ord,Typeable,Data,Generic)

instance Show (Path b t) where
  show (Path x) = show x
