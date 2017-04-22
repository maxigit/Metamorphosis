{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module ExampleSpec where

import Test.Hspec
import Lens.Micro
import Metamorphosis
import Control.Monad

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

spec = do
  styleSpecs

 
