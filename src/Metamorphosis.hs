{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Metamorphosis where

import Language.Haskell.TH

-- | map

reshape :: ((String,  [String]) -> Maybe (String, [String]) ) -> Name -> Q [Dec]
reshape f recName  = do
  let a = mkName "A"
  return [DataD [] a [] Nothing
                [NormalC a [(Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Int)]]
                []
         ]
reshape f recName  = [d|data A = A Int|]

inject = error "FIX ME"

extract = error "FIX ME"


sequence =  error "FIX ME"
