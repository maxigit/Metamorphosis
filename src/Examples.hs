{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Metamorphosis
import qualified Examples.Data as D
import Data.Char
import Data.Maybe
import Control.Monad


$(metamorphosis (:[]) [''D.Unique, ''D.Record, ''D.Plain, ''D.ABC])

deriving instance Show Record
deriving instance Show Plain
deriving instance Show ABC

record = Record {code="code", price=4.5}
unique = Unique
plain = Plain "code" 4.5
abc = [A, B, C ]

$(metamorphosis (\fd -> [fd { fdTName = fdTName fd ++ "M"
                            , fdCName = fdCName fd ++ "M"
                            , fdTypes = "Maybe" : fdTypes fd
                            }]) [''D.Record])

$(metamorphosis (\fd -> [fd { fdTName = fdTName fd ++ "F"
                            , fdCName = fdCName fd ++ "F"
                            , fdTypes = "f" : fdTypes fd
                            }]) [''D.Record])


recordF :: Applicative f => RecordF f
recordF = RecordF (pure "code") (pure 4.5)
-- * Flatten SUM type
$(metamorphosis (\fd -> [ fd  { fdTName = "ABC'"
                              , fdCName = "ABC'"
                              , fdFName = Just (map toLower $ fdCName fd)
                              , fdTypes = ["Bool"]
                              }]
                 )
                 [''D.ABC]
 )
 -- * Product to Sum type
$(metamorphosis (\fd -> [ fd  { fdTName = "RSum'"
                              , fdCName = "RSum" ++ (capitalize . fromJust $ fdFName fd)
                              , fdFName = Nothing
                              }]
                 )
                 [''D.Record]
 )
-- * Enum for field
$(metamorphosis (\fd -> [ fd  { fdTName = "RField'"
                              , fdCName = "R" ++ (capitalize . fromJust $ fdFName fd) ++ "E"
                              , fdFName = Nothing
                              , fdTypes = []
                              }]
                 )
                 [''D.Record]
 )
-- * Product to many classes
$(metamorphosis (\fd -> [ fd  { fdTName = "R" ++ (capitalize . fromJust $ fdFName fd)
                              , fdCName = "R" ++ (capitalize . fromJust $ fdFName fd)
                              , fdFName = Nothing
                              } ]
                 )
                 [''D.Record]
 )

-- * Recordize
$(metamorphosis (\fd -> [ fd  { fdTName = "PlainR"
                              , fdCName = "PlainR"
                              , fdFName = map Just ["code", "price"] !! (fdPos fd - 1)
                              } ]
                 )
                 [''D.Plain]
 )

plainr = PlainR {code = "plain", price =10}


-- -- * Merge to record
-- should be a  setter 
$(metamorphosis (fdName "RecordQ") [''D.Record, ''D.Quantity])
deriving instance Show RecordQ
recordq = RecordQ { code = "q", quantity = 10, price = 2.3}
$(metamorphosis (fdName "RecordPLain") [''D.Record, ''D.Plain])

-- * filter fields
$(metamorphosis (fdName "RecordSmall" >=> \fd -> if fdFName fd == Just "price"
                              then []
                              else [fd]
                )
                [''D.Record]
 )

$(generateExtract (fdName "RecordSmall" >=> \fd -> if fdFName fd == Just "price"
                              then []
                              else [fd]
                )
                [''D.Record]
                [''RecordSmall]
                "extractSmal"
 )
                
