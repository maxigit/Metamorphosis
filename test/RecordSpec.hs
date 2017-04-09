{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
module RecordSpec where

import Control.Lens
import Test.Hspec
import Metamorphosis

data Product = Product { style :: String
                       , variation :: String
                       , price :: Double
                       , quantity :: Int
                       } deriving (Show, Eq)

-- | Generates Style class
-- data Style = Style { style :: String
--                    , price :: Double
--                    } deriving (Show, Eq)
$(metamorphosis
   ( (\fd -> if fd ^. fdFName `elem` map Just ["variation", "quantity"]
             then []
             else [fd])
   . (fdTName .~ "Style")
   . (fdCName .~ "Style")
   )
   [''Product]
 )
deriving instance Show Style
deriving instance Eq Style
 

productToStyle = undefined
styleInProduct = undefined

spec :: Spec
spec = do
  describe "Style" $ do
    let product = Product "style" "var" 15.50 2 
    it "gets" $ do
      productToStyle product `shouldBe` (Style "style" 15.50)
    it "sets" $ do
      styleInProduct (Style "new" 7) product `shouldBe` (Product "new" "var" 7.0 2)





-- main  :: IO ()
