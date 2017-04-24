{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module ExampleSpec where

import Test.Hspec
import Lens.Micro
import Metamorphosis
import Control.Monad
import Metamorphosis.Applicative
import Data.Functor.Identity
import Control.Applicative

-- * Simple record
data Product = Product { style :: String
                       , variation :: String
                       , price :: Double
                       , quantity :: Int
                       } deriving (Show, Eq)

-- | Generates Style type
-- data Style = Style { style :: String
--                    , price :: Double
--                    } deriving (Show, Eq)
$(metamorphosis
   ( (\fd -> if fd ^. fdFieldName `elem` map Just ["variation", "quantity"]
             then []
             else [fd])
   . (fdTConsName .~ "Style")
   )
   [''Product]
   (\n -> if n `elem` ["ProductToStyle", "Product'StyleToProduct"]
          then (Just identityBCR)
          else Nothing
   )
   (const [''Show, ''Eq])
 )

styleSpecs = 
  describe "Style" $ do
    it "gets a Style from a Product" $ do
      iProductToStyle (Product "style" "var" 15.5 2 ) `shouldBe` (Style "style" 15.50) 
    it "sets a Style inside a Product" $ do
      iProduct'StyleToProduct (Product "style" "var" 15.0 2) (Style "new" 7) `shouldBe` (Product "new" "var" 7.0 2)

-- * Record parametrized by a functor.
data Record = Record { code :: String, price :: Double} deriving (Read, Show, Eq, Ord)
-- | Generates parametric version of Record
-- Unfortunately we can't derives Eq and Show in the general case, so we'll have to
-- do it manually.
-- data RecordF f = RecordF f { code :: f String, price :: f Double}
$(metamorphosis
   ( return
   . (fdTConsName .~ "RecordF")
   . (fdTypes %~ ("f":))
   )
   [''Record]
   (const (Just applicativeBCR))
   (const [])
 )

deriving instance Show (RecordF Maybe)
deriving instance Eq (RecordF Maybe)
deriving instance Show (RecordF [])
deriving instance Eq (RecordF [])

recordFSpec =
  describe "Parametric records" $ do
    it "builds an applicative version " $ do
      aRecordToRecordF (Record "T-Shirt" 7) `shouldBe` Identity (RecordF (Just "T-Shirt") (Just 7))
    it "traverses when all values are present" $ do
      aRecordFToRecord (RecordF (Just "T-Shirt") (Just 7)) `shouldBe` Just (Record "T-Shirt" 7) 
    it "doesn't traverse when on value is missing" $ do
      aRecordFToRecord (RecordF (Just "T-Shirt") Nothing) `shouldBe` Nothing
    it "generates copy" $ do
      aRecordFToRecordF (RecordF (Just "T-Shirt") (Just 7))  `shouldBe` Identity (RecordF ["T-Shirt"] [7])
    it "traverse ZipList" $ do
      aRecordFToRecord (RecordF ["T-Shirt", "Cap"] [7, 2.5])  `shouldBe`
        ZipList [(Record ("T-Shirt") 7), (Record ("Cap") 2.5 )]

-- * Many to Many
data AB = AB { a :: Int, b :: Maybe Int} deriving (Eq, Show)
data C = C { c :: Int } deriving (Eq, Show)
-- Transforms AB,C to A,BC
-- data A {a :: Int}, data BC { b :: Int, c :: Int }
$(metamorphosis 
  (\fd -> [ fd & (case _fdFieldName fd of
                    Just "a" -> fdTConsName .~ "A"
                    Just "b" -> fdTConsName .~ "BC"
                    Just "c" -> (fdTConsName .~ "BC") . (fdPos .~ 3)
                 )
          ]
  )
 [''AB, ''C]
 (const (Just applicativeBCR))
 (const [''Show, ''Eq])
 )
manyToManySpec = describe "Many to Many" $ do
  it "splits from (AB,C) to (A,BC)" $ do
    aAB'CToA'BC (AB 1 (Just 2) ) (C 3) `shouldBe` ((Identity (A 1), Identity (BC (Just 2) 3)))
  it "extracts A" $  do
    aABToA (AB 1 (Just 2)) `shouldBe` Identity (A 1)
  it "sets C into BC" $ do
    aBC'CToBC (BC (Just 1) 0 ) (C 3) `shouldBe` Identity (BC (Just 1) 3)
  it "used Nothing when value are missing" $ do
    aCToBC (C 3) `shouldBe` Identity (BC Nothing 3)
  it "extracts C from a BC" $ do
    aBCToC (BC Nothing 7) `shouldBe` Identity (C 7)

  
spec = do
  styleSpecs
  recordFSpec
  manyToManySpec


 

