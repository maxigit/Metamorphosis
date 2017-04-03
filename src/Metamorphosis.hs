{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Control.Monad
import Data.Char
import Data.List (sort, nub, group, groupBy)
import Data.Function (on)
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace

type TName = String -- Type name
type CName = String -- Constructor name
type FName = String -- field name
-- | map

reshape :: ((String,  [String]) -> Maybe (String, [String]) ) -> Name -> String -> Q [Dec]
reshape f recName new  = do
  recInfo <- reify recName
  return [ go recInfo ]
  
  where go (TyConI (DataD context name vars kind constructors derivings )) =
           (DataD context newName vars kind (map renameCons constructors) derivings )
        newName = mkName new

        reshapeBangs = mapMaybe (reshapeBang f)
        reshapeVarBangs = mapMaybe (reshapeVarBang f)

        renameCons (NormalC name bangs) = NormalC (rename name) (reshapeBangs bangs)
        renameCons (RecC name bangs) = RecC (rename name) (reshapeVarBangs bangs)
        -- renameCons (InfixC bang name bang') = InfixC  (reshapeBang bang) (rename name) (reshapeBang bang')
        renameCons (ForallC vars ctx con) = ForallC vars ctx (renameCons con)
        rendameCons (GadtC names vars typ) = GadtC (map rename names) vars typ
        rendameCons (RecGadtC names vars typ) = RecGadtC (map rename names) vars typ

        rename name =
          let base = nameBase name
          in mkName $ case f (base, []) of
                          Nothing -> base ++ "'"
                          Just (new, _) -> new
            
      
reshapeBang :: ((String,  [String]) -> Maybe (String, [String]) ) -> BangType -> Maybe BangType
reshapeBang  f (bang, typ) =
  (\(_, b', t') -> (b', t')) `fmap` reshapeVarBang f (mkName "", bang, typ)

reshapeVarBang :: ((String,  [String]) -> Maybe (String, [String]) ) -> VarBangType -> Maybe VarBangType
reshapeVarBang  f (name, bang, typ) =
  let base = nameBase name
  in case f (base, typeToChain typ) of
          Nothing -> Nothing
          Just (new, types) -> Just ( mkName new
                                    , bang
                                    , fromJust $ chainToType types
                                    )
     

typeToChain :: Type -> [String]
typeToChain (ForallT _ _ typ ) = typeToChain typ
typeToChain (AppT f t) = typeToChain f ++ typeToChain t
typeToChain (SigT t _ ) = typeToChain t
typeToChain (VarT n) = [nameBase n]
typeToChain (ConT t) = [nameBase t]
typeToChain (PromotedT t) = [nameBase t]
typeToChain info = error $ "typeToChain not implemented for :" ++ show info
             
chainToType :: [String] -> Maybe Type
chainToType [] = Nothing
chainToType [t] | isVar t = Just $ VarT (mkName t)
                | otherwise = Just $ ConT (mkName t)
chainToType (t:ts) =  liftM2 AppT (chainToType [t]) (chainToType ts)
  

isVar [] = False
isVar (c:cs) = isLower c

sequence =  error "FIX ME"

-- * TH
-- ** Type
data FieldDesc = FieldDesc
  { fdTName :: String -- ^ Type name
  , fdCName :: String -- ^ Constructor name
  , fdPos :: Int -- ^ position within the constructor
  , fdFName :: Maybe String -- ^ Field Name if Recorder
  , fdBang :: Bang
  , fdTypes :: [String] -- ^ [] means no field at all (ex Enum)
  , fdMName :: Maybe String -- ^ Module name . only used to use proper constructor name

  } deriving (Show, Eq, Ord)

-- | poor man refinement type
-- Just a way to tell that some functions want or generate sorted list
data GroupedByType = GroupedByType (Maybe String) String [FieldDesc]
data GroupedByCons = GroupedByCons (Maybe String) String [FieldDesc]

-- ** Function
-- | The workhorse of this package. Transform a set of data types to another set of data types
-- The mapping between old fields to new is done by a mapping function.
-- It can be used to copy a class, split it to multiple ones, split to sum type
-- aggregate sum type to product types etc ...
metamorphosis :: (FieldDesc -> [FieldDesc]) -> [Name] -> Q [Dec]
metamorphosis f names = do
  infos <- mapM reify names
  let fields = concatMap collectFields infos
      newFields = concatMap f fields
  return $ map generateType (groupByType newFields)

collectFields :: Info -> [FieldDesc]
collectFields (TyConI (DataD cxt tName vars kind cons cxt')) = concatMap go cons where
  go (NormalC cName []) =  [ FieldDesc { fdTName = nameBase tName
                                     , fdCName = nameBase cName
                                     , fdFName = Nothing
                                     , fdPos = 0
                                     , fdBang = (Bang NoSourceUnpackedness NoSourceStrictness)
                                     , fdTypes = []
                                     , fdMName = nameModule tName
                                     }
                           ]
  go (NormalC cName bangs) =  zipWith (go' cName) bangs [1..]
  go (RecC cName varbangs) =  zipWith (go'' cName) varbangs [1..]
  go (InfixC bang cName bang') = [ go' cName bang 1
                                 , go' cName bang' 2
                                 ]

  go' cName (bang, typ) pos = FieldDesc { fdTName = nameBase tName
                                    , fdCName = nameBase cName
                                    , fdFName = Nothing
                                    , fdPos  = pos
                                    , fdBang = bang
                                    , fdTypes = typeToChain typ
                                    , fdMName = nameModule tName
                                    }

  go'' cName (fName, bang, typ) pos = (go' cName (bang, typ) pos) {fdFName = Just (nameBase fName)}
collectFields info = error $ "collectFields only works with type declaration." ++ show ( ppr info )

generateType :: GroupedByType -> Dec
generateType group@(GroupedByType mName tName fields) = let
  cons = groupByCons group
  vars = nub $ sort (concatMap getVars fields) 
  in DataD [] (mkName tName) vars Nothing (map generateCons cons) []

generateCons :: GroupedByCons -> Con
generateCons (GroupedByCons mName cName fields) = let
  sorted = sort fields -- sort by position
  cname = mkName (capitalize cName)
  -- check all fields have a name or not
  in case traverse toVarBangType fields of
    Nothing -> NormalC cname (mapMaybe toBangType fields)
    Just [] -> NormalC cname []
    Just varbangs -> RecC (mkName cName) (varbangs)

toBangType :: FieldDesc -> Maybe BangType
toBangType field = fmap (\t -> (fdBang field, t)) (toType field)
toVarBangType :: FieldDesc -> Maybe VarBangType
toVarBangType field = case fdFName field of
  Nothing -> Nothing
  Just name -> fmap (\t -> (mkName name, fdBang field, t)) (toType field)

toType :: FieldDesc -> Maybe Type
toType field = chainToType (fdTypes field)

-- | Extract parametric variables from a field
getVars :: FieldDesc -> [TyVarBndr]
getVars field = let
  vars = filter (isVar) (fdTypes field)
  in map (PlainTV . mkName) vars
   
groupByType :: [FieldDesc] -> [GroupedByType]
groupByType fields =  let
  sorted = sort fields
  groups = groupBy ((==) `on` fdTName) fields
  in [GroupedByType (fdMName master) (fdTName master) group | group <- groups, let master = head group ]
groupByCons :: GroupedByType -> [GroupedByCons]
groupByCons  (GroupedByType _ _ fields)= let
  sorted = sort fields
  groups = groupBy ((==) `on` fdCName) fields
  in [GroupedByCons (fdMName master) (fdCName master) group | group <- groups, let master = head group ]


printDecs :: String ->  Q [Dec] ->  Q [Dec]
printDecs name qDecs = do
  decs <- qDecs
  let str = show decs
  sq <- [|str|]
  return [ ValD (VarP (mkName name)) (NormalB sq) []]
  


-- * Helper

capitalize [] = []
capitalize (c:cs) = toUpper c : cs

-- ** FieldDesc Combinator
fdName :: String -> FieldDesc -> [FieldDesc]
fdName name fd = [fd {fdTName = name, fdCName = name }]


-- * convert
-- | a contains e. We can extract an e from a a and therefore reinject it.
class Has a e where
  extract :: a -> e
  inject :: a -> e -> a

  -- law -- extract = id
  -- inject a . extract = id

-- | Generates an extract function from a set of types to a set of types
-- example XYZ -> (XY, Z)
-- will generate extract (X x y z) = (XY x y, Z z)
generateExtract :: (FieldDesc -> [FieldDesc]) -> [Name] -> [Name] -> String ->  DecsQ
generateExtract f as bs fname = do
  aInfos <- mapM reify as
  bInfos <- mapM reify bs
  
  let aFields = concatMap collectFields aInfos
      bFields = concatMap collectFields bInfos
      bnames = map nameBase bs

  -- we need to generate all constructors combinations on the input side
      clauses = buildExtractClauses f aFields bnames bFields
  traceShowM ("bFields", bFields)
  return [ FunD (mkName fname) clauses ]

buildExtractClauses :: (FieldDesc -> [FieldDesc]) -> [FieldDesc] -> [String] -> [FieldDesc] -> [Clause]
buildExtractClauses f fields targets bfields = let
  -- | Transform a field and check if matches the required targets
  trans :: FieldDesc -> Maybe [FieldDesc]
  trans field = let newFields = f field
                in if all ((`elem` targets) . fdTName) newFields
                      then Just newFields
                      else Nothing
  groups = groupByType fields
  -- generates all constructor combinations
  go :: [GroupedByCons] -> [GroupedByType] -> [Clause]
  go [] [] = error $ "Can't generate extract function for " ++ show fields
  go typeCons [] = maybeToList $ buildExtractClause f targets typeCons (groupByType bfields)
  go typeCons (group:groups) = do -- []
    cons <- groupByCons group
    go (cons:typeCons) groups 
  in go [] groups 

buildExtractClause :: (FieldDesc -> [FieldDesc]) -> [String] -> [GroupedByCons] -> [GroupedByType] -> Maybe Clause
buildExtractClause f names groups btypes =  let
  pats = [ConP (mkName cname) (fieldPats fields)  | (GroupedByCons mname cname fields) <- groups]
  fieldPats fields = map fieldPat fields
  fieldPat field = case f field of
    [] -> WildP
    _ -> VarP (fdPatName field)

  fields = concat [ fields | (GroupedByCons _ _ fields) <- groups ]
  newFields = recomputeFDPositions (concatMap f fields)
  fieldAssoc = Map.fromList [traceShow ("field", field, "new", new) ((fdCName new, fdPos new), fdPatName field) | (field, new) <- zip fields newFields ]
  body = TupE (map (consBody fieldAssoc) btypes)
  -- in traceShowId $ Just $ Clause (map ParensP pats) (NormalB body) []
  result = Clause (map ParensP pats) (NormalB body) []
  in traceShow ("fields", fields) $ traceShow ("result", show $ ppr result) (Just result)

-- | Recalculates the position of a field relative to its constructor
-- exapmle [2,5,10] -> [1,2,3]
recomputeFDPositions :: [FieldDesc] -> [FieldDesc]
recomputeFDPositions fields = let
  groups = map (map go . groupByCons) (groupByType fields)
  go group@(GroupedByCons _ _ fields) = zipWith (\fd i -> fd { fdPos = i }) fields [1..]
  in concatMap (concat) groups

fdPatName :: FieldDesc -> Name
fdPatName field = mkName $ fromMaybe ("v" ++ show (fdPos field)) (fdFName field)

consBody :: Map (String, Int) Name -> GroupedByType -> Exp
consBody varMap (GroupedByType mname typ fields) = let
  vars = map findVar fields 
  findVar fd = case Map.lookup ((fdCName fd, fdPos fd)) varMap of
    Nothing -> error $ "can't extract field " ++ show fd
    Just vname -> VarE vname
  in traceShow (varMap, fields) $ foldl AppE (ConE (mkName $ maybe "" (++ "." ) mname ++ typ)) vars
 -- pat = patTuble 
  -- A x
  -- A y
  -- A s -- not used
  -- B z
  -- B Nothing

  -- A x, B z -> AB (Just x) Nothing (Just z)
  -- A x, B Nothing -> AB (Just x) Nothing Nothing
  -- A s , ... -> not used
  -- A y, B z -> AB Nothing (Just y) (Just z)
  -- A y, B Nothing -> AB Nothing (Just y) Nothing
  
  

  
  
-- | like inject but whereas inject is losless, tranfer isn't.
-- There is no way to extract the inject result.

class Transfer a e where
  transfer :: a -> e -> a
  recover :: a -> Maybe e
 
-- * rseq
-- R f => f R  Identity


-- * rmap
-- R -> (forall x . x -> a) -> [a]


  
