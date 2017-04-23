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

data Product = Product { style :: String
                       , variation :: String
                       , price :: Double
                       , quantity :: Int
                       } deriving (Show, Eq)

p = Product "style" "var" 15.50 2 

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
   (\n -> if n `elem` ["ProductToStyle", "ProductStyleToProduct"]
          then (Just identityBCR)
          else Nothing
   )
   (const [''Show, ''Eq])
 )

styleSpecs = 
  describe "Style" $ do
    it "gets from a p" $ do
      iProductToStyle p `shouldBe` (Style "style" 15.50)
    it "sets to a p" $ do
      iProductStyleToProduct p (Style "new" 7) `shouldBe` (Product "new" "var" 7.0 2)

data Record = Record { code :: String, price :: Double} deriving (Read, Show, Eq, Ord)
-- | Generates parametric version of Record
-- Unfortunately we can't derives Eq and Show in the general case, so we'll have to
-- do it mahttps://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/nually.
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

  
spec = do
  styleSpecs
  recordFSpec

 
