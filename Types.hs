{-# LANGUAGE TemplateHaskell #-}
module Metarmorphosis.Types where

import Lens.Micro
import Lens.Micro.TH
import Control.Monad

type ModuleName = String
type TypeName = String
type ConsName = String
type FieldName = String

-- | Denormalized description of a field including constructor, type etc ...
-- For example, data A = A { x :: Int} will have one field A.A.x [Int]
data FieldDesc = FieldDesc
  { _fdTypeName :: String -- ^ Type name
  , _fdConsName :: String -- ^ Constructor name
  , _fdPos :: Int -- ^ position within the constructor
  , _fdFieldName :: Maybe String -- ^ Field Name if Recorder
  , _fdBang :: Bang
  , _fdTypes :: [String] -- ^ [] means no field at all (ex Enum)
  , _fdMName :: Maybe String -- ^ Module name . only used to use proper constructor name

  } deriving (Show, Eq, Ord)

makeLenses ''FieldDesc

-- | Traversal to set both name at the same time
fdTConsName  :: Traversal' FieldDesc String
fdTConsName f fd = let
  fConsName = f (fdConsName fd)
  fTypeName = f (fdTypeName fd)
  in (\tname cname -> fd { _fdTname = tname,  _fdConsName = cname }  ) <*> fConsName fTypeName


-- | Key to identify uniquely a Field
_fdKey :: FieldDesc -> (Maybe ModuleName, ConsName, Either Pos FieldName)
_fdKey field = ( _fdMName fd
               , _fdConsName fd
               , maybe (Left (_fdPos fd)) Right (_fdFieldName fd)
               )

-- | Denormalized description of a Type
data TypeDesc = TypeDesc { _tdName :: TypeName
                         , _tdCons :: [ConsDesc]
                         , _tdModuleName :: Maybe ModuleName
                         } deriving (Show, Eq, Ord)

-- Denormalized description of a data constructor.
-- All field are supposed to have their type an cons field set correctly
data  ConsDesc = ConsDesc { _cdName :: ConsName
                          , _cdFields :: [Fielddesc]
                          } deriving (Show, Eq, Ord)

