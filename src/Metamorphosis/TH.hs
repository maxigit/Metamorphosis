{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis.TH where
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.List (sort, nub, group, groupBy, intercalate)
import           Data.Function (on)

import Metamorphosis.Types


-- | Retrieves the field descriptions of data type (from its Info)
collectFields :: Info -> [FieldDesc]
collectFields (TyConI (DataD cxt tName vars kind cons cxt')) = concatMap go cons where
  go (NormalC cName []) =  [ FieldDesc { _fdTypeName = nameBase tName
                                     , _fdConsName = nameBase cName
                                     , _fdFieldName = Nothing
                                     , _fdPos = 0
                                     , _fdBang = (Bang NoSourceUnpackedness NoSourceStrictness)
                                     , _fdTypes = []
                                     , _fdModuleName = nameModule tName
                                     }
                           ]
  go (NormalC cName bangs) =  zipWith (go' cName) bangs [1..]
  go (RecC cName varbangs) =  zipWith (go'' cName) varbangs [1..]
  go (InfixC bang cName bang') = [ go' cName bang 1
                                 , go' cName bang' 2
                                 ]

  go' cName (bang, typ) pos = FieldDesc { _fdTypeName = nameBase tName
                                    , _fdConsName = nameBase cName
                                    , _fdFieldName = Nothing
                                    , _fdPos  = pos
                                    , _fdBang = bang
                                    , _fdTypes = typeToTypeNames typ
                                    , _fdModuleName = nameModule tName
                                    }

  go'' cName (fName, bang, typ) pos = (go' cName (bang, typ) pos) {_fdFieldName = Just (nameBase fName)}
collectFields info = error $ "collectFields only works with type declaration." ++ show ( ppr info )

-- | Structure Field Descriptions to types
fieldsToTypes :: [FieldDesc] -> [TypeDesc]
fieldsToTypes fds = let
  sorted = sort fds -- by type, constructor, pos
  groups = groupBy ((==) `on` _fdType) sorted
  mkType fields@(fd:_) = let typeD = TypeDesc (_fdTypeName fd)
                                 (_fdModuleName fd)
                                 (fieldsToConss typeD fields)
                         in typeD
  in map mkType groups


fieldsToConss :: TypeDesc -> [FieldDesc] -> [ConsDesc]
fieldsToConss typD fds = let
  sorted = sort fds -- by type, constructor, pos
  groups = groupBy ((==) `on` _fdConsName) sorted
  mkCons fields@(fd:_) = let consD = ConsDesc (_fdConsName fd)
                                              typD
                                              ( zipWith3 FieldDescPlus (sort fields)
                                                                       (repeat consD)
                                                                       (repeat [])
                                              )
                         in consD
  in map mkCons groups

-- | Converts a type (AST) to a list of String (chain)
-- ex: Maybe Int -> ["Maybe", "Int"]
typeToTypeNames :: Type -> [String]
typeToTypeNames (ForallT _ _ typ ) = typeToTypeNames typ
typeToTypeNames (AppT f t) = typeToTypeNames f ++ typeToTypeNames t
typeToTypeNames (SigT t _ ) = typeToTypeNames t
typeToTypeNames (VarT n) = [nameBase n]
typeToTypeNames (ConT t) = [nameBase t]
typeToTypeNames (PromotedT t) = [nameBase t]
typeToTypeNames (ListT) = ["[]"]
typeToTypeNames info = error $ "typeToTypeNames not implemented for :" ++ show info
             
