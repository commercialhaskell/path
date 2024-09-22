{-# LANGUAGE TypeApplications #-}

module Utils
    ( typeableToType
    ) where

import Data.Bifunctor (first)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Typeable (splitTyConApp)
import qualified Language.Haskell.TH.Syntax as TH
import Type.Reflection

typeableToType :: (Typeable a) => proxy a -> TH.Type
typeableToType = typeRepToType . someTypeRep

typeRepToType :: SomeTypeRep -> TH.Type
typeRepToType rep =
    uncurry (List.foldl' f)
        . first tyConToType
        . splitTyConApp
        $ rep
    where
        f :: TH.Type -> SomeTypeRep -> TH.Type
        f memo = TH.AppT memo . typeRepToType

        tyConToType :: TyCon -> TH.Type
        tyConToType tc =
            (if isType then TH.ConT else TH.PromotedT)
                ( TH.Name
                    (TH.OccName (List.dropWhile (== '\'') (tyConName tc)))
                    ( TH.NameG
                        (if isType then TH.TcClsName else TH.DataName)
                        (TH.PkgName (tyConPackage tc))
                        (TH.ModName (tyConModule tc))
                    )
                )

        isType :: Bool
        isType = someTypeRepKind rep == SomeTypeRep (typeRep @Type)

someTypeRepKind :: SomeTypeRep -> SomeTypeRep
someTypeRepKind (SomeTypeRep rep) = SomeTypeRep (typeRepKind rep)
