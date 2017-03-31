{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Metamorphosis
import qualified Examples.Data as D
import Data.Char
import Data.Maybe


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
                              , fdCName = "R" ++ ( fromJust $ fdFName fd)
                              , fdFName = Nothing
                              }]
                 )
                 [''D.Record]
 )
-- * Product to many classes

