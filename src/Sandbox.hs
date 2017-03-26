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

genF' :: String -> ExpQ
genF' y = do
  let y' = mkName y
  let x = mkName "x"
  return $ LamE [VarP x] (InfixE (Just $ VarE x) (VarE (mkName "+")) (Just $ VarE y'))
