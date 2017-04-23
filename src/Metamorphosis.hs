{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Metamorphosis
( metamorphosis
, identityBCR
, applicativeBCR
, extractBCR
, module Metamorphosis.Types
) where

import Metamorphosis.Applicative
import Metamorphosis.TH
import Metamorphosis.Types
import Metamorphosis.Internal
import Language.Haskell.TH
import Data.Maybe
import Data.List(subsequences)
import Lens.Micro
import Debug.Trace

metamorphosis :: (FieldDesc -> [FieldDesc])
              -> [Name]
              -> (String -> Maybe BodyConsRules)
              -> (String -> [Name])
              -> Q [Dec]
metamorphosis f sources rulesF deriv = do
  sourceTypes <- collectTypes sources
  let targetTypes = applyFieldMapping f sourceTypes
      typeDecls = map (\t -> generateType (t ^. tdName . to deriv) t) targetTypes

      sourceTypes' = reverseTypeDescs targetTypes

  converters <- genConverters rulesF targetTypes
  converters' <- genConverters rulesF sourceTypes'
  copiers <- genCopiers rulesF targetTypes
  copiers' <- genCopiers rulesF sourceTypes'

  return $ typeDecls ++ converters ++ converters' ++ copiers ++ copiers'


-- | Generates all possible converter between different classes
-- We generate all possible combinations and filter the name 
genConverters :: (String -> Maybe BodyConsRules)
              ->  [TypeDesc] -> Q [Dec]
genConverters rulesF targets' = do
  let -- sources =  typeSources targets
      -- add field to 
      autoSource fp = case fp ^. fpSources of
        [] -> fp & fpSources .~ [fp]
        _ -> fp
      unnest :: [[TypeDesc]] -> [[TypeDesc]]
      unnest ts = map return (concat ts)
      targets = targets' & mapped . tdCons . mapped . cdFields . mapped  %~ autoSource

      subsequences' = (Prelude.filter (not  . null)) . subsequences
      combinations = 
        [ (rules, name, srcs, tgts)
        | tgts <- subsequences' targets
        , let sources = typeSources tgts
        , srcs' <- subsequences' sources
        , let srcs = [ [s] | s <- srcs'] :: [[TypeDesc]]
        , let name = traceShowId $ converterBaseName (map (map _tdName) srcs) (tgts ^.. each . tdName)
        , rules <- maybeToList $ rulesF name
        ]
  mapM (\(rules, name, srcs, tgts) -> genConversion name rules (unnest srcs) tgts) combinations

-- | Generates converter to itself
-- Useful for parametric types, to convert between same type
-- but changing the type parameter

genCopiers rulesF targets' =  genConverters rulesF targets where
  targets = targets' & mapped . tdCons. mapped. cdFields . mapped . fpSources .~ []
