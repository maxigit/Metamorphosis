{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Metamorphosis
(
) where

import Metamorphosis.Applicative
import Metamorphosis.TH
import Metamorphosis.Types
import Metamorphosis.Internal
import Language.Haskell.TH
import Data.Maybe
import Data.List(subsequences)
import Lens.Micro

metamorphosis :: BodyConsRules
              -> (FieldDesc -> [FieldDesc])
              -> [Name]
              -> (String -> Maybe String)
              -> Q [Dec]
metamorphosis rules f sources filter = do
  sourceTypes <- collectTypes sources
  let targetTypes = applyFieldMapping f sourceTypes
      typeDecls = map (generateType []) targetTypes

      sourceTypes' = reverseTypeDescs targetTypes

  converters <- genConverters rules filter targetTypes
  converters' <- genConverters rules filter sourceTypes'

  return $ typeDecls ++ converters ++ converters'


-- | Generates all possible converter between different classes
-- We generate all possible combinations and filter the name 
genConverters :: BodyConsRules
              -> (String -> Maybe String)
              ->  [TypeDesc] -> Q [Dec]
genConverters rules filter targets = do
  let sources =  typeSources targets
      combinations :: [(String, [TypeDesc], [TypeDesc])]
      combinations = catMaybes
        [ (, srcs, tgts) `fmap` newName
        | srcs <- subsequences sources
        , tgts <- subsequences targets
        , let name = (rules ^. bcrFunName) [srcs ^.. each . tdName] (tgts ^.. each . tdName)
        , let newName = filter name
        ]
  mapM (\(name, srcs, tgts) -> genConversion rules [srcs] tgts) combinations
