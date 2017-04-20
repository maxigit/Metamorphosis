module Metamorphosis.Util where

import Data.Char(toUpper, toLower)

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

uncapitalize :: [Char] -> [Char]
uncapitalize [] = []
uncapitalize (c:cs) = toLower c : cs
