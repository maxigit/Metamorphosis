{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis.TH where
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import Metamorphosis.Internal
import Metamorphosis.Util
import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro
import Data.List(sort)


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
genConsClause :: BodyConsRules -> [[ConsDesc]] -> [ConsDesc] -> Q Clause
genConsClause rules sources targets = do
  fieldToName <- genNameMap  (sources ^. each . each . cdFields )
  -- filter constructor which are actually not used
  let used = targets ^. each . cdFields . each  . fpSources
  let fieldToName0 = Map.filterWithKey (\f n -> f `elem` used) fieldToName
      pats = map (conssToTuplePat fieldToName0) sources
      body = TupE (map (genConsBodyE rules fieldToName) targets)
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

 -- | Generates a call to a constructor
-- ex: (consF A <op> fieldF a <op> fieldF b)
genConsBodyE :: BodyConsRules -> Map FieldDescPlus Name -> ConsDesc -> Exp
genConsBodyE (BodyConsRules consF fieldsF opFs) fieldToName consDesc = let
  cons = consF $  ConE (mkName $ consDesc ^. cdName)
  fieldEs = map fieldToE (consDesc ^. cdFields)
  fieldToE fp = let
    sources = sort $ fp ^. fpSources
    vars = map (\f -> maybe (TupE []) VarE (Map.lookup f fieldToName)) sources
    in fieldsF vars
  in foldl (\m (exp, op) -> m `op` exp )cons (zip fieldEs opFs)


-- ** Default body constructor rules
fieldsToTuples = go where
  go [] = TupE []
  go [f] = ParensE $ f 
  go fs = foldl AppE (VarE $ tupleDataName (length fs)) fs

fieldsToTuples' name fs = fieldsToTuples'' (VarE $ mkName name) fs
fieldsToTuples'' expr fs = fieldsToTuples $ map (expr `AppE`) fs
opToFunction op a b= UInfixE a (VarE $ mkName op) b
identityBCR = BodyConsRules id fieldsToTuples (repeat AppE)
applicativeBCR = BodyConsRules id fieldsToTuples (map opToFunction $ "<$>":repeat "<*>")
extractBCR = BodyConsRules id (fieldsToTuples' "extract") (map opToFunction $ "<$>":repeat "<*>")
monoidBCR f = BodyConsRules (const (VarE $ mkName "mempty"))
                            (fieldsToTuples' f)
                            (map opToFunction $ repeat "<>")
monoidPureBCR f = BodyConsRules (const (VarE $ mkName "mempty"))
                            (fieldsToTuples'' (ParensE $ UInfixE (VarE $ mkName "pure")
                                                                 (VarE $ mkName ".")
                                                                 (VarE $ mkName f)
                                              )
                            )
                            (map opToFunction $ repeat "<>")

                

