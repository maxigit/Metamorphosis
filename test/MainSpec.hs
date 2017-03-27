{-# LANGUAGE TemplateHaskell #-}
module MainSpec where

import Metamorphosis
import Test.Hspec
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr

-- * Dummy data
data A = A0
data F a = F0 a
data R0 = R1 { x :: Int, y :: Double}


spec :: Spec
spec = do
  describe "Test TH" $ do
    it "copies A" $ do
      let q = reshape Just ''A
      r <- runQ q    
      (show . ppr ) r `shouldBe` "data A = A0"
    it "copies F" $ do
      let q = reshape Just ''F
      r <- runQ q    
      (show . ppr ) r `shouldBe` "data F a = F0 a"

main :: IO ()
main = hspec spec
