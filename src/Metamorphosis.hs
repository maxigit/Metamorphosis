{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Control.Monad
import Data.Char
import Data.List (sort, nub, group, groupBy)
import Data.Function (on)

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

inject = error "FIX ME"

extract = error "FIX ME"


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

  } deriving (Show, Eq, Ord)

-- | poor man refinement type
-- Just a way to tell that some functions want or generate sorted list
data GroupedByType = GroupedByType String [FieldDesc]
data GroupedByCons = GroupedByCons String [FieldDesc]

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
                                    }

  go'' cName (fName, bang, typ) pos = (go' cName (bang, typ) pos) {fdFName = Just (nameBase fName)}
collectFields info = error $ "collectFields only works with type declaration." ++ show ( ppr info )

generateType :: GroupedByType -> Dec
generateType group@(GroupedByType tName fields) = let
  cons = groupByCons group
  vars = nub $ sort (concatMap getVars fields) 
  in DataD [] (mkName tName) vars Nothing (map generateCons cons) []

generateCons :: GroupedByCons -> Con
generateCons (GroupedByCons cName fields) = let
  sorted = sort fields -- sort by position
  -- check all fields have a name or not
  in case traverse toVarBangType fields of
    Nothing -> NormalC (mkName cName) (mapMaybe toBangType fields)
    Just [] -> NormalC (mkName cName) []
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
  in [GroupedByType (fdTName (head group)) group | group <- groups ]
groupByCons :: GroupedByType -> [GroupedByCons]
groupByCons  (GroupedByType _ fields)= let
  sorted = sort fields
  groups = groupBy ((==) `on` fdCName) fields
  in [GroupedByCons (fdCName (head group)) group | group <- groups ]


printDecs :: String ->  Q [Dec] ->  Q [Dec]
printDecs name qDecs = do
  decs <- qDecs
  let str = show decs
  sq <- [|str|]
  return [ ValD (VarP (mkName name)) (NormalB sq) []]
  

