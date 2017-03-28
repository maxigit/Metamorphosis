{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Control.Monad
import Data.Char

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
reshapeBang  f bang = Just bang

reshapeVarBang :: ((String,  [String]) -> Maybe (String, [String]) ) -> VarBangType -> Maybe VarBangType
reshapeVarBang  f (name, bang, typ) =
  let base = nameBase name
  in case f (base, typeToChain typ) of
          Nothing -> Nothing
          Just (new, types) -> Just ( mkName new
                                    , bang
                                    , chainToType types
                                    )
     

typeToChain :: Type -> [String]
typeToChain (ForallT _ _ typ ) = typeToChain typ
typeToChain (AppT f t) = typeToChain f ++ typeToChain t
typeToChain (SigT t _ ) = typeToChain t
typeToChain (VarT n) = [nameBase n]
typeToChain (ConT t) = [nameBase t]
typeToChain (PromotedT t) = [nameBase t]
typeToChain t = error $ "typeToChain not implemented for :" ++ show t
             
chainToType :: [String] -> Type
chainToType [] = error "shouldn't not happen"
chainToType [t] | isVar t = VarT (mkName t)
                | otherwise = ConT (mkName t)
chainToType (t:ts) =  AppT (chainToType [t]) (chainToType ts)
  

isVar [] = False
isVar (c:cs) = isLower c

inject = error "FIX ME"

extract = error "FIX ME"


sequence =  error "FIX ME"
