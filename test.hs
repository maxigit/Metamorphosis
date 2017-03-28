{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Metamorphosis

data A = A
data F a b = F a b
data B = B Int
data R = R { name :: String, price :: Double} deriving (Eq, Ord, Read, Show)
$(reshape (const Nothing) ''A "A0")
$(reshape (\(n, v) -> Just ("Blob" ++ n, v)) ''B "BLOB")
$(reshape (\(n, v) -> Just (n ++ "Z", v)) ''R "Rob")
-- Change all types to Int
$(reshape (\(n, v) -> Just (n ++ "Int",[ "Int"])) ''R "RobInt")
-- Add Maybe 
$(reshape (\(n, v) -> Just (n ++ "M",  "Maybe": v)) ''R "Mob")
-- Remove type constructors
$(reshape (\(n, v) -> Just (n ++ "U",  [last v])) ''Mob "Uob")
