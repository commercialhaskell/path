{-
Include.hs expects the quasiquoter to be named 'pstr', however despite the fact that both
System.OsPath.Posix and System.OsPath.Windows export a quasiquoter named 'pstr',
System.OsPath names it 'osp'. So we just re-export with a different name.
-}
module OsPath.Internal.OsPathWrapper (module System.OsPath, pstr) where

import System.OsPath hiding (osp)
import qualified System.OsPath
import Language.Haskell.TH.Quote (QuasiQuoter)

pstr :: QuasiQuoter
pstr = System.OsPath.osp
