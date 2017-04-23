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
-- do it manually.
-- data RecordF f = RecordF f { code :: f String, price :: f Double}
$(metamorphosis
   ( return
   . (fdTConsName .~ "RecordF")
   . (fdTypes %~ ("f":))
   )
   [''Record]
   -- (const (Just extractBCR))
   (const (Just extractBCR))
   (const [])
 )

deriving instance Show (RecordF Maybe)
deriving instance Eq (RecordF Maybe)
deriving instance Show (RecordF [])
deriving instance Eq (RecordF [])

recordFSpec =
  describe "Parametric record" $ do
    it "builds an applicative version " $ do
      eRecordToRecordF (Record "T-Shirt" 7) `shouldBe` Identity (RecordF (Just "T-Shirt") (Just 7))
    it "traverses when all values are present" $ do
      eRecordFToRecord (RecordF (Just "T-Shirt") (Just 7)) `shouldBe` Just (Record "T-Shirt" 7) 
    it "doesn't traverse when on value is missing" $ do
      eRecordFToRecord (RecordF (Just "T-Shirt") Nothing) `shouldBe` Nothing
    it "generates copy" $ do
      eRecordFToRecordF (RecordF (Just "T-Shirt") (Just 7))  `shouldBe` Identity (RecordF ["T-Shirt"] [7])
      

  
spec = do
  styleSpecs
  recordFSpec

 
