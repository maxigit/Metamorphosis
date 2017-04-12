-- | Main functions to manipulate what's in Types.
-- and doesn't need Template Haskell
module Metamorphosis.Internal
( fieldsToTypes
, module Metamorphosis.Types
)
where

import Lens.Micro
import Data.Function (on)
import Data.List (sort, nub, group, groupBy, intercalate)
import Metamorphosis.Types
import qualified Data.Map as Map
import Data.Map(Map)

-- | Structure Field Descriptions to types
fieldsToTypes :: [FieldDesc] -> [TypeDesc]
fieldsToTypes fds = let
  sorted = nub $ sort fds -- by type, constructor, pos
  groups = groupBy ((==) `on` _fdType) sorted
  mkType fields@(fd:_) = let typeD = TypeDesc (_fdTypeName fd)
                                 (_fdModuleName fd)
                                 (fieldsToConss typeD fields)
                         in typeD
  in map mkType groups


-- | Create ConsDesc from a list of FieldDesc belonging to the same type.
fieldsToConss :: TypeDesc -> [FieldDesc] -> [ConsDesc]
fieldsToConss typD fds = let
  sorted = nub $ sort fds -- by type, constructor, pos
  groups = groupBy ((==) `on` _fdConsName) sorted
  mkCons fields@(fd:_) = let consD = ConsDesc (_fdConsName fd)
                                              typD
                                              ( zipWith3 FieldDescPlus (sort fields)
                                                                       (repeat consD)
                                                                       (repeat [])
                                              )
                         in consD
  in map mkCons groups

-- |
-- | Apply a field Mapping function to a list of TypeDesc.
applyFieldMapping :: (FieldDesc -> [FieldDesc]) -> [TypeDesc] -> [TypeDesc]
applyFieldMapping fieldMapping typDs = let
  fields =  typDs ^. each.tdCons.each.cdFields
  -- We need to create the new fields
  -- but also keep track of their sources
  newFields'Source = [ (_fdKey new , (new, [source]) )
                     |  source <- fields
                     , let field = source ^. fpField
                     , new <- fieldMapping  field
                     ]
  -- Group all sources
  newMap = Map.fromListWith (\(n, ss) (_, ss') -> (n, ss++ss')) newFields'Source
  setSources fp = fp { _fpSources = maybe []
                                          snd
                                          (Map.lookup (fp ^. to _fpKey) newMap)
                     }
  newTypes0 = fieldsToTypes (map fst (Map.elems newMap))
  -- We need to update the source
  in newTypes0 & mapped.tdCons.mapped.cdFields.mapped %~ setSources
  

-- | Extract the sources and recompute the source
-- so they point to the original target.
-- It should be equivalent to applying the inverse of a fieldMapping to types
-- (which were target when calling applyingFieldMapping)
-- prop  rev.rev = id
reverseTypeDescs :: [TypeDesc] -> [TypeDesc]
reverseTypeDescs typDs = let
  originalTargets = typDs ^. each.tdCons.each.cdFields

  sources'target = [(_fpKey source, (source, [originalTarget]))
                   | originalTarget <- originalTargets
                   , source <- _fpSources originalTarget
                   ]

  sourcesMap = Map.fromListWith (\(n, ss) (_,ss') -> (n, ss++ss')) sources'target
  setSources fp = fp { _fpSources = maybe []
                                          snd
                                          (Map.lookup (fp ^. to _fpKey) sourcesMap)
                     }
  newTypes0 = fieldsToTypes (map (_fpField.fst) (Map.elems sourcesMap))
  in newTypes0 & mapped.tdCons.mapped.cdFields.mapped %~ setSources

  

  
