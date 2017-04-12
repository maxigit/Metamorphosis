{-# LANGUAGE TemplateHaskell #-}
module Metamorphosis.Types where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH.Syntax
import Lens.Micro
import Lens.Micro.TH

type ModuleName = String
type TypeName = String
type ConsName = String
type FieldName = String

-- | Denormalized description of a field including constructor, type etc ...
-- For example, data A = A { x :: Int} will have one field A.A.x [Int]
-- This type is only there for the main user (to be use in mapping function).
data FieldDesc = FieldDesc
  { _fdTypeName :: String -- ^ Type name
  , _fdConsName :: String -- ^ Constructor name
  , _fdPos :: Int -- ^ position within the constructor
  , _fdFieldName :: Maybe String -- ^ Field Name if Recorder
  , _fdBang :: Bang
  , _fdTypes :: [String] -- ^ [] means no field at all (ex Enum)
  , _fdModuleName :: Maybe String -- ^ Module name . only used to use proper constructor name

  } deriving (Show, Eq, Ord)

makeLenses ''FieldDesc

-- | Traversal to set both name at the same time
fdTConsName  :: Traversal' FieldDesc String
fdTConsName f fd = let
  fConsName = f (_fdConsName fd)
  fTypeName = f (_fdTypeName fd)
  in (\tname cname -> fd { _fdTypeName = tname,  _fdConsName = cname }  ) <$> fConsName <*> fTypeName


-- | Key to identify uniquely a Field
newtype FieldKey = FieldKey (Maybe ModuleName, ConsName, Either Int FieldName) deriving (Show, Eq, Ord)
_fdKey :: FieldDesc -> FieldKey
_fdKey fd = FieldKey ( _fdModuleName fd
                     , _fdConsName fd
                     , maybe (Left (_fdPos fd)) Right (_fdFieldName fd)
                     )

_fdType :: FieldDesc -> (Maybe ModuleName, TypeName)
_fdType = liftA2 (,) _fdModuleName _fdTypeName
-- | Denormalized description of a Type
data TypeDesc = TypeDesc { _tdName :: TypeName
                         , _tdModuleName :: Maybe ModuleName
                         , _tdCons :: [ConsDesc]
                         } deriving (Show, Eq, Ord)

_tdFields typD = concatMap (_cdFields) (_tdCons typD)
tdFields = to _tdFields

-- Denormalized description of a data constructor.
-- All field are supposed to have their type an cons field set correctly
data  ConsDesc = ConsDesc { _cdName :: ConsName
                          , _cdTypeDesc :: TypeDesc
                          , _cdFields :: [FieldDescPlus]
                          } deriving (Show, Eq, Ord)

-- | FieldDesc + internal information
data FieldDescPlus = FieldDescPlus { _fpField :: FieldDesc
                                   , _fpCons :: ConsDesc
                                   , _fpSources :: [FieldDescPlus]
                                   } deriving (Show, Eq, Ord)

_fpKey :: FieldDescPlus -> FieldKey
_fpKey = _fdKey . _fpField
makeLenses ''TypeDesc
makeLenses ''ConsDesc
makeLenses ''FieldDescPlus