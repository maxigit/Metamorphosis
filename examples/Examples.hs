{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Metamorphosis
import qualified Examples.Data as D
import qualified Examples.Data as Examples.Data
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Lens
import Data.Functor.Identity
  

$(metamorphosis (:[]) [''D.Unique, ''D.Record, ''D.Plain, ''D.ABC])

deriving instance Show Record
deriving instance Show Plain
deriving instance Show ABC

record = Record {code="code", price=4.5}
unique = Unique
plain = Plain "code" 4.5
abc = [A, B, C ]

$(metamorphosis (\fd -> [fd { _fdTName = _fdTName fd ++ "M"
                            , _fdCName = _fdCName fd ++ "M"
                            , _fdTypes = "Maybe" : _fdTypes fd
                            }]) [''D.Record])

$(metamorphosis (\fd -> [fd { _fdTName = _fdTName fd ++ "F"
                            , _fdCName = _fdCName fd ++ "F"
                            , _fdTypes = "f" : _fdTypes fd
                            }]) [''D.Record])
$(generateExtract (\fd -> [fd { _fdTName = _fdTName fd ++ "F"
                            , _fdCName = _fdCName fd ++ "F"
                            , _fdTypes = "f" : _fdTypes fd
                            }]) [''Examples.Data.Record] [''RecordF] "extractFF")
$(generateExtract (\fd -> [fd { _fdTName = init $ _fdTName fd
                            , _fdCName = init $ _fdCName fd
                            , _fdTypes = drop 1 $ _fdTypes fd
                            }]) [''RecordF] [''D.Record] "extractFFF")

-- instance Monad f => ExtractF Examples.Data.Record f (RecordF f) where
--    extractF = extractFF

instance Monad f => ExtractF (RecordF f) f D.Record where
   extractF = extractFFF


recordF :: Applicative f => RecordF f
recordF = RecordF (pure "code") (pure 4.5)
-- * Flatten SUM type
$(metamorphosis (\fd -> [ fd  { _fdTName = "ABC'"
                              , _fdCName = "ABC'"
                              , _fdFName = Just (map toLower $ _fdCName fd)
                              , _fdTypes = ["Bool"]
                              }]
                 )
                 [''D.ABC]
 )
 -- * Product to Sum type
$(metamorphosis (\fd -> [ fd  { _fdTName = "RSum'"
                              , _fdCName = "RSum" ++ (capitalize . fromJust $ _fdFName fd)
                              , _fdFName = Nothing
                              }]
                 )
                 [''D.Record]
 )
-- * Enum for field
$(metamorphosis (\fd -> [ fd  { _fdTName = "RField'"
                              , _fdCName = "R" ++ (capitalize . fromJust $ _fdFName fd) ++ "E"
                              , _fdFName = Nothing
                              , _fdTypes = []
                              }]
                 )
                 [''D.Record]
 )
-- * Product to many classes
$(metamorphosis (\fd -> [ fd  { _fdTName = "R" ++ (capitalize . fromJust $ _fdFName fd)
                              , _fdCName = "R" ++ (capitalize . fromJust $ _fdFName fd)
                              , _fdFName = Nothing
                              } ]
                 )
                 [''D.Record]
 )

-- * Recordize
$(metamorphosis (\fd -> [ fd  { _fdTName = "PlainR"
                              , _fdCName = "PlainR"
                              , _fdFName = map Just ["code", "price"] !! (_fdPos fd - 1)
                              } ]
                 )
                 [''D.Plain]
 )

plainr = PlainR {code = "plain", price =10}


-- -- * Merge to record
-- should be a  setter 
$(metamorphosis (_fdName "RecordQ" . (\fd -> fd {_fdFName = fmap (++"Q") (_fdFName fd )})) [''D.Record, ''D.Quantity])
$(generateExtract (\fd -> let typ = if _fdFName fd == (Just "quantityQ")
                                        then "Quantity"
                                        else "Record"
                          in [ fd { _fdMName = Just "D"
                                  , _fdTName =  typ
                                  , _fdCName = typ
                                  , _fdFName = fmap init (_fdFName fd)
                                  }
                             ] )
                  [''RecordQ]
                  [''D.Record, ''D.Quantity]
                  "extractQ")
deriving instance Show RecordQ
recordq = RecordQ { codeQ = "q", quantityQ = 10, priceQ = 2.3}
$(metamorphosis (_fdName "RecordPLain") [''D.Record, ''D.Plain])
-- * filter fields
$(metamorphosis (_fdName "RecordSmall" >=> \fd -> if _fdFName fd == Just "price"
                              then []
                              else [fd]
                )
                [''D.Record]
 )
deriving instance Show RecordSmall

$(generateExtract (_fdName "RecordSmall" >=> \fd -> if _fdFName fd == Just "price"
                              then []
                              else [fd]
                )
                [''D.Record]
                [''RecordSmall]
                "extractSmall"
 )
rsmall = runIdentity $ extractSmall record
                

main :: IO ()
main = putStrLn "Everything is Ok"


-- * Trying to implement lens manually
-- ** RecordQ -> (Record, Quantity)
-- lens
recordL :: Functor f => (Record -> f Record) -> RecordQ -> f RecordQ
recordL f rq@(RecordQ code price quantity) = let
  recordF = f (Record code price) 
  injectR (Record code' price') = RecordQ code' price' quantity
  in fmap injectR  recordF

-- lens
quantityL :: Functor f => (D.Quantity -> f D.Quantity) -> RecordQ -> f RecordQ
quantityL f rq@(RecordQ code price quantity) = let
  quantityF = f (D.Quantity quantity)
  injectR (D.Quantity quantity') = RecordQ code price quantity'
  in fmap injectR  quantityF
  

-- **  (Record, Quantity) -> RecordQ
-- ** (Record, Quantity) <-> RecordQ
rqL :: Functor f => ((Record , D.Quantity) -> f (Record, D.Quantity)) -> RecordQ -> f RecordQ
rqL f rq@(RecordQ code price quantity) = let
  r'q = f (Record code price, D.Quantity quantity)
  injectR (Record code' price', D.Quantity quantity') = RecordQ code' price' quantity'
  in fmap injectR  r'q

-- dimap (a -> b) -> (c -> d) -> p b c     -> p a d
-- Iso ::                        p a (f b) -> p s (f t)
--  a -> s
--  b -> a
--  c -> f b
-- d -> f t
-- dimap :: (s -> a) -> (f b -> f t)
  -- s = t =  RecordQ
  -- a = b = (R, Q)

-- could be written using : iso r'qTorq rqTor'q. No need for lenses there.
rqI :: Iso' RecordQ (Record, D.Quantity)
rqI p = dimap sToa fbToft p where
  sToa (RecordQ code price quantity) = (Record code price, D.Quantity quantity)
  fbToft = fmap  bTot
  bTot (Record code price, D.Quantity quantity) = RecordQ code price quantity

-- in term of previous lenses

-- RecordF
-- recF :: (Applicative f, Functor f, Functor g) => (Record -> f Record) -> RecordF g -> f (RecordF g)
recF f (RecordF codeF priceF) = let
  rf = Record <$> codeF <*> priceF
  rf' = traverse f rf
  in undefined -- fmap (\(Record code price) -> Record code price) rf'
