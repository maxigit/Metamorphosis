{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Metamorphosis
( metamorphosis
, identityBCR
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

metamorphosis :: BodyConsRules
              -> (FieldDesc -> [FieldDesc])
              -> [Name]
              -> (String -> Bool)
              -> (String -> [Name])
              -> Q [Dec]
metamorphosis rules f sources filter deriv = do
  sourceTypes <- collectTypes sources
  let targetTypes = applyFieldMapping f sourceTypes
      typeDecls = map (\t -> generateType (t ^. tdName . to deriv) t) targetTypes

      sourceTypes' = reverseTypeDescs targetTypes

  converters <- genConverters rules filter targetTypes
  converters' <- genConverters rules filter sourceTypes'

  return $ typeDecls ++ converters ++ converters'


-- | Generates all possible converter between different classes
-- We generate all possible combinations and filter the name 
genConverters :: BodyConsRules
              -> (String -> Bool)
              ->  [TypeDesc] -> Q [Dec]
genConverters rules filter targets = do
  let -- sources =  typeSources targets
      subsequences' = (Prelude.filter (not  . null)) . subsequences
      combinations :: [(String, [[TypeDesc]], [TypeDesc])]
      combinations = 
        [ (name, srcs', tgts)
        -- | srcs <- subsequences' sources
        | tgts <- subsequences' targets
        , let sources = typeSources tgts
        , srcs <- subsequences' sources
        , tgts' <- [[], [tgts]] -- add target to sources to get setter
        , let srcs' = tgts' ++ [srcs]  :: [[TypeDesc]]
        , let name = traceShowId $ (rules ^. bcrFunName) (map (map _tdName) srcs') (tgts ^.. each . tdName)
        , filter name
        ]
  mapM (\(name, srcs, tgts) -> genConversion rules srcs tgts) combinations
