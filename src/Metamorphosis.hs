{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Metamorphosis
( metamorphosis
, mmZip
, mmZipN
, identityBCR
, applicativeBCR
, monoidBCR
, monoidPureBCR
, module Metamorphosis.Types
) where

import Metamorphosis.Applicative
import Metamorphosis.TH
import Metamorphosis.Types
import Metamorphosis.Internal
import Language.Haskell.TH
import Data.Maybe
import Data.List(subsequences, (\\))
import Lens.Micro

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


inspectTypes  :: [TypeDesc] -> Q ()
inspectTypes typs = do
  let fields = typs ^. each . tdCons . each . cdFields
      display fd = (fd ^. fpField . fdFieldName, fd ^. fpSources . each . fpCons .  cdName )
  return ()

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
      targets = targets' -- & mapped . tdCons . mapped . cdFields . mapped  %~ autoSource

      subsequences1 = (Prelude.filter (not  . null)) . subsequences
      combinations = 
        -- @TODO Rewrite
        [ (rules, name, srcs, tgts1)
        | tgts <- subsequences1 targets
        , let sources = typeSources tgts
        , srcs' <- subsequences1 sources
        -- Remove the sources which are not relevant anymore
        , let tgts1 = (filterSourceByTypes srcs' tgts) &
                    mapped . tdCons . mapped . cdFields . mapped %~ autoSource
        , let setterTypes = typeSources tgts1 & (filter (`notElem` srcs'))
        , extra <- subsequences setterTypes
        , let srcs = [ [s] | s <- extra ++ srcs'] :: [[TypeDesc]]
        , let name = converterBaseName (map (map _tdName) srcs) (tgts1 ^.. each . tdName)
        , rules <- maybeToList $ rulesF name
        ]
  mapM (\(rules, name, srcs, tgts) -> genConversion name rules (unnest srcs) tgts) combinations

-- | Generates converter to itself
-- Useful for parametric types, to convert between same type
-- but changing the type parameter

genCopiers rulesF targets' = concat `fmap` mapM go targets 
  where go target = genConverters rulesF [target]
        targets = targets' & mapped . tdCons. mapped. cdFields . mapped %~ auto
        auto fp = fp & fpSources .~ [fp & fpSources .~ []]

mmZip :: String -> Name -> Q [Dec]
mmZip fname typeName = mmZipN 2 fname typeName Nothing

mmZipN :: Int ->  String -> Name -> Maybe Name -> Q [Dec]
mmZipN n fname typeName consName = do
  let suffix 2 = ""
      suffix n = show n
  [td] <- collectTypes [typeName]
  (:[]) <$> genZip n (fname ++ (nameBase typeName) ++ suffix n) fname td consName
