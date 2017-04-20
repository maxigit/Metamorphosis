{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis.TH where
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import Metamorphosis.Internal
import Metamorphosis.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro


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
             

-- * Generating Code
-- | Generates a pattern from a constructor
-- ex: data A = A Int String
-- >> a@(A a1 a2)
consToPat :: (Map FieldDescPlus Name) -> ConsDesc -> Pat
consToPat fieldToName consDesc = let
  pats = map  fieldName (consDesc ^. cdFields)
  fieldName field = maybe WildP VarP (Map.lookup field fieldToName)
  in ConP (mkName $ consDesc ^. cdName) pats

  
  

-- | Generates a tuple patter from a list of constructors
-- ex: [(A Int), (B String)] -> (A a, B b)
conssToTuplePat :: (Map FieldDescPlus Name) -> [ConsDesc] -> Pat
conssToTuplePat fieldToName consDescs = let
  in TupP (map (consToPat fieldToName) consDescs )

-- | Generates a constructor clause of the form
-- (A a) (XY x y) = (f AXZ  (g a) (g x), f Y (g y)
genConsClause :: [[ConsDesc]] -> [ConsDesc] -> Q Clause
genConsClause sources targets = do
  fieldToName <- genNameMap  (sources ^. each . each . cdFields )
  let pats = map (conssToTuplePat fieldToName) sources
  let body = TupE (map (genConsBodyE  fieldToName) targets)
  return $  Clause pats (NormalB body) []



-- | Generates an map of uncapturable pattern (argument name)
-- from a list of constructors
genNameMap :: [FieldDescPlus] -> Q (Map FieldDescPlus Name)
genNameMap fields = do
    pats <- mapM (genName) fields 
    return $ Map.fromList (zip fields pats)


genName :: FieldDescPlus -> Q Name
genName fp = newName $ case (fp ^. fpField . fdFieldName) of
    Nothing -> uncapitalize $ fp ^. fpCons . cdName ++ show (fp ^. fpField . fdPos)
    Just name -> name

genConsBodyE :: Map FieldDescPlus Name -> ConsDesc -> Exp
genConsBodyE fieldToName consDesc = let
  cons =  ConE (mkName $ consDesc ^. cdName)
  fieldEs = map fieldToE (consDesc ^. cdFields)
  fieldToE fp = maybe (TupE []) VarE (Map.lookup fp fieldToName)
  in foldl AppE cons fieldEs



