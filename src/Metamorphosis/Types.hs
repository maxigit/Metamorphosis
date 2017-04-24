{-# LANGUAGE TemplateHaskell #-}
module Metamorphosis.Types where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH.Syntax
import Data.List (intercalate)
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

-- | Create a field, mainly for testing purpose.
mkField :: String -> Int -> [Char] -> String -> FieldDesc
mkField cname pos fname types =
  FieldDesc cname cname pos (toMaybe fname) bang (words types) Nothing
  where bang = (Bang NoSourceUnpackedness NoSourceStrictness)
        toMaybe "" = Nothing
        toMaybe s = Just s

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
                         } deriving (Show) -- Eq, Ord)
typeDescToTuple = (,) <$> _tdName <*> _tdModuleName
instance Eq TypeDesc where a == b = typeDescToTuple a == typeDescToTuple b
instance Ord TypeDesc where compare a b = compare (typeDescToTuple a) (typeDescToTuple b)

_tdFields typD = concatMap (_cdFields) (_tdCons typD)
tdFields = to _tdFields

_tdFullName td = maybe "" (++".") (_tdModuleName td) ++ _tdName td

-- Denormalized description of a data constructor.
-- All field are supposed to have their type an cons field set correctly
-- As it's cyclic structure it can't be shown properly.
data  ConsDesc = ConsDesc { _cdName :: ConsName
                          , _cdTypeDesc :: TypeDesc
                          , _cdFields :: [FieldDescPlus]
                          }


-- | ConsDesc can be cyclic, so we need a non-cyclic version
-- to be able to instantiate Ord, Eq and Show
consDescToTuple cd = ("ConsDesc", _cdFullName cd, map fieldDescPlusToTuple (_cdFields cd))

instance Show ConsDesc where show = show . consDescToTuple
instance Eq ConsDesc where  a== b = consDescToTuple a == consDescToTuple b
instance Ord ConsDesc where  compare a  b = compare (consDescToTuple a) (consDescToTuple b)

_cdFullName cd = _tdFullName (_cdTypeDesc cd) ++ "#" ++ _cdName cd

-- | FieldDesc + internal information
-- As it's cyclic structure it can't be shown properly.
-- We define a fake Show instance which doesn't display cycles
data FieldDescPlus = FieldDescPlus { _fpField :: FieldDesc
                                   , _fpCons :: ConsDesc
                                   , _fpSources :: [FieldDescPlus]
                                   }

fieldDescPlusToTuple fp = ( "FieldDescPlus"
                      , _fpField fp
                      , _cdFullName (_fpCons fp)
                      , _fpSources fp
                      )
instance Show FieldDescPlus where show = show . fieldDescPlusToTuple
instance Eq FieldDescPlus where  a== b = fieldDescPlusToTuple a == fieldDescPlusToTuple b
instance Ord FieldDescPlus where  compare a  b = compare (fieldDescPlusToTuple a) (fieldDescPlusToTuple b)
                             
_fpKey :: FieldDescPlus -> FieldKey
_fpKey = _fdKey . _fpField
makeLenses ''TypeDesc
makeLenses ''ConsDesc
makeLenses ''FieldDescPlus
-- | Defines how body constructor are generated.
data BodyConsRules = BodyConsRules { _bcrProcessCons :: Exp -> Exp
                               , _bcrProcessFields :: [Exp] -> Exp
                               , _bcrSeparator :: [Exp ->  Exp -> Exp]
                               , _bcrFunName :: String -> String
                               }

makeLenses '' BodyConsRules
