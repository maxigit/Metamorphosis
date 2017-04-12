{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis.TH where
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import Metamorphosis.Internal


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

collectTypes :: [Name] -> Q [TypeDesc]
collectTypes names = do
  infos <- mapM reify names
  let fields = concatMap collectFields infos
  return $ fieldsToTypes fields


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
             
