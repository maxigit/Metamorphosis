{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
-- | Just trying things ...
module Sandbox where

import Language.Haskell.TH 
-- * Generate simple types

-- | Generate manually
-- data Unique = Unique
genUnique :: Q Dec
genUnique = do
--   *Sandbox Sandbox> runQ [d|data A = B|]
-- [DataD [] A_2 [] Nothing [NormalC B_3 []] []]
  let a = mkName "A"
  return $ DataD [] a [] Nothing [NormalC a []] [ ]


-- genU' a = [d|data A = $$(undefined) |]
-- same using QQ
 
-- * Generating simple functions
genF :: String -> ExpQ
genF y = [| \x -> x+ $(varE (mkName y)) |]

genF' :: String -> ExpQ
genF' y = do
  let y' = mkName y
  let x = mkName "x"
  return $ LamE [VarP x] (InfixE (Just $ VarE x) (VarE (mkName "+")) (Just $ VarE y'))


showType :: Name ->  ExpQ
showType a = do
  info <- reify (a)
  let str = show info
  [| str |]
  
