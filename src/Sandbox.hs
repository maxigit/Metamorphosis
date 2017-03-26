{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- | Just trying things ...
module Sandbox where

import Language.Haskell.TH 
-- * Generate simple types

-- | Generate manually
-- data Unique = Unique
-- genUnique = TyConI undefined


-- same using QQ
 
-- * Generating simple functions
genF = [| \x -> x+1 |]

genF' :: ExpQ
genF' = do
  let x = mkName "x"
  return $ LamE [VarP x] (InfixE (Just $ VarE x) (VarE (mkName "+")) (Just $ LitE (IntegerL 5)))
