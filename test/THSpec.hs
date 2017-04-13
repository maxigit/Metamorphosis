{-# LANGUAGE TemplateHaskell #-}
module THSpec where

import Metamorphosis.TH
import Metamorphosis.Types
import Test.Hspec
import Lens.Micro
import Language.Haskell.TH

data A = A Int String | Record { style :: String , price :: Double} deriving Show
aInfo = TyConI (DataD []
               (mkName "A")
               []
               Nothing [ NormalC (mkName "A")
                         [ (Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "Int"))
                         , (Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "String"))
                         ]
                       , RecC (mkName "Record") [ ((mkName "style") ,Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "String"))
                                                , ((mkName "price"),Bang NoSourceUnpackedness NoSourceStrictness,ConT (mkName "Double"))
                                                ]
                       ]
          [ConT (mkName "Show")]
        )


spec :: Spec
spec = do
  describe "collectFields" $ do
    let aFields = collectFields aInfo
    it "collect fields for sum types and records" $ do
      aFields ^.. each . fdTypeName `shouldBe` ["A", "A", "A", "A"]
      aFields ^.. each . fdConsName `shouldBe` ["A", "A", "Record", "Record"]
      aFields ^.. each . fdFieldName `shouldBe` [Nothing, Nothing, Just "style", Just "price"]
