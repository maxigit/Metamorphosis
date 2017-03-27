{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis where

import Language.Haskell.TH

-- | map

reshape :: ((String,  [String]) -> Maybe (String, [String]) ) -> Name -> String -> Q [Dec]
reshape f recName new  = do
  recInfo <- reify recName
  return [ go recInfo ]
  
  where go (TyConI (DataD context name vars kind constructors derivings )) =
           (DataD context newName vars kind (map renameCons constructors) derivings )
        newName = mkName new

        renameCons (NormalC name bang) = NormalC (rename name) bang
        renameCons (RecC name bangs) = RecC (rename name) bangs
        renameCons (InfixC bang name bang') = InfixC bang (rename name) bang'
        renameCons (ForallC vars ctx con) = ForallC vars ctx (renameCons con)
        rendameCons (GadtC names vars typ) = GadtC (map rename names) vars typ
        rendameCons (RecGadtC names vars typ) = RecGadtC (map rename names) vars typ

        rename name =
          let base = nameBase name
          in mkName $ case f (base, []) of
                          Nothing -> base ++ "'"
                          Just (new, _) -> new
            
      
inject = error "FIX ME"

extract = error "FIX ME"


sequence =  error "FIX ME"
